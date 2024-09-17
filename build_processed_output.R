library(tidyverse)
library(glue)
library(magrittr)
library(lubridate)
library(officer)

source("./helpers/getFileTibble.R")
source("./helpers/word2tibble.R")
source("./helpers/medications2tibble.R")
source("./helpers/lab2tibble.R")
source("./helpers/find_invalid_case_data.R")
source("./helpers/readSummaries.R")
source("./helpers/merge_and_format_data.R")
source("./helpers/convert2markdown_list.R")
source("./helpers/merge_into_single_markdown.R")

# Create directories if they don't exist
dir.create("data/processed", showWarnings = FALSE)
dir.create("data/processed/raw", showWarnings = FALSE)
dir.create("data/processed/markdown", showWarnings = FALSE)
dir.create("data/processed/merged", showWarnings = FALSE)

# Get raw data tibble
data2save <- getFileTibble() |>
  select(path, specialty, case_id, type, language) |>
  pivot_wider(names_from = type, values_from = path) |>
  group_by(specialty, case_id) |>
  mutate(across(c(Facts, Labs), \(x) if_else(is.na(x), x[language == "original"], x))) |>
  distinct(Facts, Labs, Main, Summary, language) |>
  ungroup() |>
  mutate(id = row_number()) |>
  nest(data = c(Facts, Labs, Main, Summary, case_id, specialty, language)) |>
  pull(data) |>
  lapply(as.list) |>
  lapply(\(x) {
    merge_and_format_data(x) |>
      structure(specialty = x$specialty, case_id = x$case_id, language = x$language)
  })

# Check for errors in the data
errors <- lapply(data2save, find_invalid_case_data) |> unlist()
if (length(errors) > 0) {
  cat("Errors found:\n", errors |> paste(collapse = "\n"))
}

# Save multiple versions of the data
walk(data2save,
     function(x) {
       # Save to JSON for web interface
       jsonlite::toJSON(x) |>
         write_file(file = with(attributes(x),
                                glue("data/processed/raw/raw_{specialty}_{case_id}_{language}.json")))

       # Save to JSON for llm use with markdown formatting for tables
       convert2markdown_list(x) |>
         jsonlite::toJSON(auto_unbox = TRUE) |>
         write_file(file = with(attributes(x),
                                glue("data/processed/markdown/markdown_{specialty}_{case_id}_{language}.json")))

       # Save to single markdown file
       merge_into_single_markdown(x) |>
         write_file(file = with(attributes(x), 
                                glue("data/processed/merged/merged_{specialty}_{case_id}_{language}.md")))       
     })

data2save |> map_chr(\(x) attr(x, "language")) |> unique()

if (FALSE) {
  ortho_cases <- data2save[which(sapply(data2save, \(x) attributes(x)$specialty == "Orthopaedics" & attributes(x)$language == "original"))]
  jsonlite::toJSON(ortho_cases[[1]])

  get_attributes <- function(x) {
    tibble(specialty = attributes(x)$specialty, case_id = attributes(x)$case_id, language = attributes(x)$language)
  }

  available_charts <- lapply(data2save, get_attributes) |>
    bind_rows()


  available_charts |>
    add_count(specialty, case_id, language) |>
    filter(n > 1)

  available_charts |>
    group_by(specialty, language) |>
    summarise(n = n())

  # Illustrate the counts in a ggplot crosstable with colors for the counts
  available_charts |>
    add_count(specialty, name = "sp_count") |>
    add_count(language, name = "lang_count") |>
    mutate(specialty = fct_reorder(specialty, sp_count, .desc = TRUE),
           language = if_else(language == "original", "English (org)", language),
           language = fct_reorder(language, lang_count, .desc = FALSE),
           # English (org) is always the last language (on top in the graph)
           language = fct_relevel(language, "English (org)", after = Inf)) |>
    group_by(specialty, language) |>
    summarise(n = n(), .groups = "drop") |>
    ggplot(aes(x = specialty, y = language, fill = n)) +
    geom_tile() +
    geom_text(aes(label = n), color = "black") +
    scale_fill_continuous(low = "#EEEEEEAA", high = "#4682B4AA", guide = guide_colorbar(title = "No translations")) +
    theme_minimal() +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab("Specialty") +
    ylab("Language")
}
