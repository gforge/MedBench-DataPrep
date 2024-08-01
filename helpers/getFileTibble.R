library(tidyverse)

#' Retrieve a tibble with information about files in a catalogue
#'
#' This function retrieves a tibble containing information about files in a specified catalogue.
#'
#' @return A tibble with information about the files in the catalogue.
#' @return A tibble with columns:
#' \describe{
#'  \item{path}{The path to the file.}
#'  \item{specialty}{The specialty of the case.}
#'  \item{case_id}{The ID of the case.}
#'  \item{type}{The type of the file.}
#'  \item{language}{The language of the file.}
#' }
#' @export
getFileTibble <- function() {
  catalogue <- "data/Cases"
  if (!dir.exists(catalogue)) {
    stop("The catalogue ", catalogue, " can not be found")
  }

  case_files <- catalogue |>
    dir(recursive = TRUE) |>
    tibble(path = _) |>
    filter(!startsWith(path, "Orthopaedics/Svenska/") &
             !str_detect(path, "Cardiology/case_[1-5]_hb/Case [1-5] Lab and Medications_hb.xlsx")) |>
    filter(endsWith(path, ".docx") | endsWith(path, ".xlsx") | endsWith(path, '.md')) |>
    mutate(filename = basename(path),
           specialty = str_replace(path, "([^/]+)/.+", "\\1"),
           case_id = case_when(str_detect(filename, "Case_\\d+@[^@]+@[^@]+\\.md") ~ str_replace(filename, "(.*)Case_(\\d+)@.+", "Case \\2"),
                               str_detect(filename, "Case_\\d+_") ~ str_replace(filename, "(.*)Case_(\\d+)_.+", "Case \\2"),
                               TRUE ~str_replace(filename, "(.*)(Case [^ ]).+", "\\2")),
           type = case_when(str_detect(tolower(filename), "fact sheet") ~ "Facts",
                            endsWith(filename, "xlsx") ~ "Labs",
                            startsWith(filename, "Summary") ~ "Summary",
                            TRUE ~ "Main"),
           language = case_when(str_detect(filename, "^Surgical Patient Case.+ EN.docx") ~ "original",
                                str_detect(filename, "^Surgical Patient Case.+ SE.docx") ~ "Swedish",
                                str_detect(path, "Svenska") ~ "Swedish",
                                str_detect(path, "Swedish") ~ "Swedish",
                                str_detect(path, "(_swe| SE)\\.(docx|xlsx)$") ~ "Swedish",
                                str_detect(path, "Danish") ~ "Danish",
                                str_detect(path, "Greek") ~ "Greek",
                                str_detect(path, "French") ~ "French",
                                str_detect(path, "German") ~ "German",
                                str_detect(path, "Italian") ~ "Italian",
                                TRUE ~ "original")) |>
    select(-filename)

  dups <- case_files |>
    add_count(specialty, case_id, type, language) |>
    filter(n > 1)
  if (nrow(dups) > 0) {
    stop("Duplicate files found:\n ", dups$path |> paste(collapse = "\n "))
  }

  return(case_files)
}
