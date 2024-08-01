#' Merge and format data
#'
#' This function takes in a data frame and performs merging and formatting operations on it.
#'
#' @param data The input case wut file-names and paths.
#'
#' @return A list containing the formatted data with the elements:
#' \describe{
#'  \item{notes}{The notes data in tibble format.}
#'  \item{medications}{The medications data in tibble format.}
#'  \item{lab}{The lab values data in tibble format.}
#'  \item{summaries}{The summaries data in tibble format.}
#' }
merge_and_format_data <- function(data) {
  tryCatch({
    word_data <- word2tibble(data$Main)
    medications_data <- medications2tibble(data$Labs, language = data$language)
    lab_values_data <- lab2tibble(data$Labs, language = data$language)
    summaries_data <- readSummaries(data$Summary)
  }, error = function(e) {
    browser()
    stop(
      "Error reading data: ", e$message,
      "\n for case: ", data$case_id, " for ", data$specialty, " (language: ", data$language, ")"
    )
  })

  full_data <- list(
    notes = word_data |> select(type, date = year, time = hour, author, content),
    medications = medications_data |> select(-timestamp))

  if (!is.null(lab_values_data)) {
    full_data$lab <- lab_values_data |> mutate(date = as.Date(timestamp), time = format(timestamp, "%H:%M")) |>
      select(-timestamp)
  }

  if (!is.null(summaries_data)) {
    full_data$summaries <- summaries_data
  }

  full_data <- Map(\(x) rename_all(x, snakecase::to_lower_camel_case), x = full_data)

  if (!all(c("notes", "medications") %in% names(full_data))) {
    stop("Failed to process ", data$case_id, " for ", data$specialty, " language: ", data$language)
  }

  return(full_data)
}