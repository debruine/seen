# display debugging messages in R (if local)
# and in the console log (if running in shiny)
debug_msg <- function(...) {
  is_local <- Sys.getenv('SHINY_PORT') == ""
  in_shiny <- !is.null(shiny::getDefaultReactiveDomain())
  txt <- toString(list(...))
  if (is_local) message(txt)
  if (in_shiny) shinyjs::runjs(sprintf("console.debug(\"%s\")", txt))
}

debug_sprintf <- function(fmt, ...) {
  debug_msg(sprintf(fmt, ...))
}

load_data <- function() { debug_msg("loading data...")
  gs4_deauth()
  url <- "https://docs.google.com/spreadsheets/d/16aZ78_QicXs6fn1fsLJefestSMpRAqjPLAVYrTBIFVc/"
  main <- read_sheet(url, skip = 2)
  spoilers <- read_sheet(url, sheet = 2, skip = 1)

   left_join(main, spoilers,
                   by = c("Title", "Year", "Do I Recommend?")) %>%
    select(title = 1,
           year = 2,
           genre = 3,
           lang = 4,
           period = 6,
           description = 7,
           where  = 8,
           rec = 9,
           happy = 10,
           death = 11) %>%
    mutate(happy = ifelse(happy == "N/A", "Not Applicable", happy),
           death = ifelse(death == "N/A", "Not Applicable", death))
}

get_opts <- function(data, col, multi = FALSE) {
  col_n <- data %>%
    select(.data[[col]])

  if (multi) {
    col_n <- col_n %>%
      separate(.data[[col]], LETTERS, sep = "\\s*(,|/)\\s*", fill = "right") %>%
      gather("x", !!col, LETTERS)
  }

  col_n <- col_n %>%
    filter(!is.na(.data[[col]])) %>%
    count(.data[[col]]) %>%
    arrange(.data[[col]])

 setNames(
   object = col_n[[col]],
   nm = sprintf("%s (%d)", col_n[[col]], col_n$n)
 )
}

filter_opts <- function(datacol, chosen, multi = FALSE) {
  # set split to a string that will never occur if not multi
  split <- if (multi) "\\s*(,|/)\\s*" else "BuzzBUZZZ"
  datacol %>%
    as.character() %>%
    sapply(strsplit, split) %>%
    sapply(function(x) {
      pattern <- paste0("^", as.character(chosen), "$")
      sapply(pattern, grepl, x) %>% any()
    }) %>%
    unname()
}

