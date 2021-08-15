# libraries ----
suppressPackageStartupMessages({
    library(shiny)
    library(shinyjs)
    library(shinydashboard)
    library(shinyWidgets)
    library(googlesheets4)
    library(DT)
    library(dplyr)
    library(tidyr)
})

# setup ----

# functions ----
source("scripts/func.R") # helper functions
full_data <- load_data()

# user interface ----

## modules ----

filterUI <- function(id, multi = FALSE) {
    ns <- NS(id)

    choices <- get_opts(full_data, id, multi)

    box(width = 12, title = textOutput(ns("title")),
        solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
        pickerInput(
            inputId = ns("options"),
            label = NULL,
            choices = choices,
            options = list(
                `actions-box` = TRUE, # select-all/deselect-all
                `live-search` = TRUE, # search the options
                `selected-text-format` = "count > 1"), # show "N options selected" for more than 1 selection
            selected = choices,
            multiple = TRUE
        )
    )
}

filterServer <- function(id, title, multi = FALSE) {
    moduleServer(id, function(input, output, session) {
        debug_sprintf("== Setting up %s Filter ==", id)

        choices <- get_opts(full_data, id, multi)

        # update choices ----
        debug_msg(id, "update choices")

        # output$title ----
        output$title <- renderText({ debug_msg(id, "title")
            if (all(choices %in% input$options)) { # all options selected
                opts <- "All"
            } else {
                opts <- paste(input$options, collapse = ", ")
            }
            sprintf("%s (%s)", title, opts)
        })

        # return vector of selected films ----
        reactive({ debug_msg(id, "filter_opts")
            filter_opts(full_data[[id]], input$options, multi)
        })
    })
}


## UI ----
ui <- dashboardPage(
    skin = "purple",
    dashboardHeader(title = "Seen and To See",
        titleWidth = "calc(100% - 44px)" # puts sidebar toggle on right
    ),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
        shinyjs::useShinyjs(),
        tags$head(
            # links to files in www/
            tags$link(rel = "stylesheet", type = "text/css", href = "basic_template.css"),
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
            tags$script(src = "custom.js")
        ),
        HTML("<div style='padding: 1em;'><a href='https://docs.google.com/spreadsheets/d/16aZ78_QicXs6fn1fsLJefestSMpRAqjPLAVYrTBIFVc/'>Spreadsheet</a> maintained by <a href='https://www.tiktok.com/@ariavelz'>@ariavelz on TikTok</a>.</div>"),

        column(width = 4,
               filterUI("genre", multi = TRUE),
               filterUI("year"),
               filterUI("lang", multi = TRUE),
               filterUI("rec"),
               filterUI("where", multi = TRUE),
               filterUI("happy"),
               filterUI("death")
        ),

        box(width = 8, title = textOutput("filter_title"),
            solidHeader = TRUE,
            checkboxGroupInput("show_cols", NULL, choices = c(), inline = TRUE),
            DTOutput("filter_table")
        )
    )
)

# server ----
server <- function(input, output, session) {
    in_genre <- filterServer("genre", "Genres", multi = TRUE)
    in_year <- filterServer("year", "Years")
    in_lang <- filterServer("lang", "Languages", multi = TRUE)
    in_rec <- filterServer("rec", "Recommended")
    in_where <- filterServer("where", "Where to Watch", multi = TRUE)
    in_happy <- filterServer("happy", "Does the queer couple end up together?")
    in_death <- filterServer("death", "Does a queer woman die?")

    # update filtered data ----
    observe({
        cols <- names(full_data)
        updateCheckboxGroupInput(
            session, "show_cols",
            choices = cols,
            selected = cols[1]
        )
    })

    filter_data <- reactive({
        # return selected rows
        sel <- in_genre() & in_year() & in_lang() & in_rec() &
               in_where() & in_happy() & in_death()
        full_data[sel, input$show_cols]
    })

    output$filter_table <- renderDT(filter_data())

    output$filter_title <- renderText({
        n_filtered_films <- nrow(filter_data())
        sprintf("Films (%s)", n_filtered_films)
    })
}

shinyApp(ui, server)
