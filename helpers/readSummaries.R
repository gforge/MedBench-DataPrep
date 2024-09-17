#' Read and process summary files
#'
#' This function reads and processes a list of summary files, extracting relevant information
#' and returning a tibble with the combined summaries.
#'
#' @param summaries A character vector of file paths to the summary files.
#'
#' @return A tibble containing the combined summaries, with columns:
#' \describe{
#'   \item{specialty}{The specialty of the case.}
#'   \item{name}{The name of the case.}
#'   \item{language}{The language of the summary.}
#'   \item{generatedBy}{The method used to generate the summary, e.g. Human.}
#'   \item{text}{The text content of the summary.}
#' }
readSummaries <- function(summaries) {
  summaries <- Filter(Negate(is.na), summaries)
  if (length(summaries) == 0) {
    return(NULL)
  }

  getSingleSummary <- function(summary) {
    txt <- summary |>
      Gmisc::pathJoin("data/Cases", fn = _) |>
      read_file() |>
      str_trim()

    core_name <- summary |>
      basename() |>
      str_remove("\\.(md|txt)$") |>
      str_remove("^Summary_4_")

    # Split core_name into its components
    if (str_detect(core_name, "@(original|Swedish)@")) {
      # E.g. original name: Summary_4_Orthopaedics_Case 1@original@gpt-4_turbo-2024-04-09@temp=0.0@basic.txt",
      regex <- "(.*)_Case (\\d+)@([^@]+)@(.*@temp=\\d\\.\\d@.*)$"
      if (!str_detect(core_name, regex)) {
        stop("Invalid file name format: ", core_name)
      }
      
      specialty <- str_match(core_name, regex)[, 2]
      case_id <- str_match(core_name, regex)[, 3]
      language <- str_match(core_name, regex)[, 4]
      generator <- str_match(core_name, regex)[, 5]
    } else {
      components <- str_split(core_name, "_")[[1]]
      
      # Extract components
      specialty <- components[1]
      case_id <- components[which(components == "Case") + 1]
      language <- components[which(components == "Case") + 2]
      generator <- ifelse(length(components) > (which(components == "Case") + 2), components[length(components)], "Human")
    }

    # Process the text content
    tibble(specialty = specialty,
           name = paste("Case", case_id),
           language = language,
           generatedBy = generator,
           text = txt)
  }

  # Read and process each summary file (we merged them with <|!!|> so that it would be easier to split them back into individual summaries)
  summary_list <- str_split(summaries, fixed("<|!!|>")) |> 
    unlist() |>
    lapply(getSingleSummary)

  # Combine all summaries into a single tibble
  final_df <- bind_rows(summary_list)
  return(final_df)
}
