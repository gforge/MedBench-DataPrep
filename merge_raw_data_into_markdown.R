library(tidyverse)
library(glue)
library(magrittr)
library(lubridate)
library(officer)

source("./helpers/getFileTibble.R")
source("./helpers/word2tibble.R")
source("./helpers/medications2tibble.R")
source("./helpers/lab2tibble.R")

process_word_data <- function(fn) {
  fn |>
    word2tibble() |>
    mutate(content = glue("# {type}, {year}, {hour}, {author}\n{content}"))
}

process_medications_data <- function(fn) {
  fn |>
    medications2tibble() |>
    select(-date) |>
    group_by(timestamp) |>
    # Build a bullet list of the medications
    summarise(content = glue(" * {Medication}, {`Way of administration`}, {Strength} {Unit} {Times_per_day} times per day") |>
                paste(collapse = "\n"),
              .groups = "drop") |>
    mutate(content = glue("# Medications, {strftime(timestamp, '%Y-%m-%d, %H:%M')}\n{content}"))
}

process_lab_values_data <- function(fn) {
  lab <- fn |>
    lab2tibble()
  if (is.null(lab)) return(NULL)

  lab |>
    group_by(timestamp) |>
    # Build a bullet list of the lab values
    summarise(content = paste0(" * ", `Lab test`, " ", Value, " ", Unit,
                               glue(" ({`Reference interval`})"),
                               collapse = "\n"),
              .groups = "drop") |>
    mutate(content = glue("# Lab values, {strftime(timestamp, '%Y-%m-%d, %H:%M')}\n{content}"))
}

merge_and_format_data <- function(data) {
  tryCatch({
    #if (data$case_id == "Case 5" & data$specialty == "Surgery") browser()
    word_data <- process_word_data(data$Main)
    medications_data <- process_medications_data(data$Labs)
    lab_values_data <- process_lab_values_data(data$Labs)
  }, error = function(e) {
    stop("Error reading data: ", e$message, "\n for case: ", data$case_id, " for ", data$specialty)
  })

  data2merge <- list(
    word_data = word_data,
    Medications = medications_data,
    Lab_values = lab_values_data
  ) |> Filter(\(x) !is.null(x), x = _)

  merged_data <- data2merge |>
    map(\(x) select(x, timestamp, content)) |>
    bind_rows() |>
    arrange(timestamp) |>
    pull(content) |>
    paste(collapse = "\n") |>
    # Fix output details
    str_replace_all(pattern = "(?<!\n)\n#", "\n\n#") |>
    str_replace_all(pattern = "\n\n\n+", "\n\n") |>
    str_replace_all(pattern = " [ ]+", " ") |>
    str_replace_all(pattern = ",,+", ",")

  return(merged_data)
}

# Create directory if it does not exist
dir.create("data/processed", showWarnings = FALSE)
data2save <- getFileTibble() |> 
  select(path, specialty, case_id, type, language) |> 
  pivot_wider(names_from = type, values_from = path) |> 
  group_by(specialty, case_id) |> 
  mutate(across(c(Facts, Labs), \(x) if_else(is.na(x), x[language == "original"], x))) |> 
  distinct(Facts, Labs, Main, language) |> 
  ungroup() |>
  mutate(id = row_number()) |> 
  nest(data = c(Facts, Labs, Main, case_id, specialty, language)) |> 
  pull(data)

data2save |>
  lapply(as.list) |>
  lapply(\(x) merge_and_format_data(x) |>
           write_file(file = with(x, 
                                  glue("data/processed/merged_{specialty}_{case_id}_{language}.md"))))
