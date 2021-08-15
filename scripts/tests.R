library(testthat)
library(dplyr)
library(googlesheets4)
source("scripts/func.R")

# filter_opts ----
test_that("filter_opts", {
  data <- load_data()

  datacol <- data[["happy"]]
  chosen <- "No"
  in_happy <- filter_opts(datacol, chosen)
  returned <- datacol[in_happy] %>% unique()
  expect_equal(chosen, returned)

  datacol <- data[["where"]]
  chosen <- "Amazon"
  in_where <- filter_opts(datacol, chosen, multi = TRUE)
  returned <- datacol[in_where] %>% unique()
  expect_true(length(returned) > 0) # write better test

  # deal with commas
  datacol <- data[["death"]]
  chosen <- "Yes (but as exposition, all other queer women are alive)"
  in_death <- filter_opts(datacol, chosen)
  expect_equal(chosen, datacol[in_death] %>% unique())
})
