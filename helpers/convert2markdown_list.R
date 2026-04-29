#' Convert data to a markdown list
#'
#' This function takes in a data object and converts it into a markdown list format.
#'
#' @param data The data object to be converted
#' @return The converted data in markdown list format
#' @examples
#' data <- c("item1", "item2", "item3")
#' convert2markdown_list(data)
#' # Output: "- item1\n- item2\n- item3"
convert2markdown_list <- function(data) {
  fixMedicationsWhenMultiples <- function(meds) {
    meds |>
      group_by(medication, wayOfAdministration, strength, unit, date) |>
      mutate(no = row_number(), n = n()) |>
      arrange(medication, wayOfAdministration, strength, unit, date) |>
      ungroup() |>
      mutate(medication = if_else(n > 1, glue("{medication} [{no}]"), medication)) |>
      select(-no, -n)
  }

  to_markdown_or_empty <- function(df) {
    if (is.null(df) || nrow(df) == 0) {
      return("")
    }

    df |>
      knitr::kable(format = "markdown") |>
      paste(collapse = "\n")
  }

  tryCatch({
    list(lab = to_markdown_or_empty(data$lab),
         singleLab = data$lab,
         medications = if (is.null(data$medications) || nrow(data$medications) == 0) {
           ""
         } else {
           data$medications |>
             fixMedicationsWhenMultiples() |>
             pivot_wider(names_from = date, values_from = timesPerDay) |>
             knitr::kable(format = "markdown") |>
             paste(collapse = "\n")
         },
         singleMedication = data$medications,
         chart = data$notes |>
           mutate(content = glue("# {type}, {date}, {time}, {author}\n{content}")) |>
           pull(content) |>
           paste(collapse = "\n\n") |>
           str_replace_all("\n{3,}", "\n\n")) |>
      map_if(\(x) inherits(x, "glue"), as.character)
  }, warning = function(w) {
    with(attributes(data),
         stop("Warning converting to markdown: ", w$message,
              "\n for case: ", case_id, " for ", specialty, " (language: ", language, ")"))
  }, error = function(e) {
    with(attributes(data),
         stop("Error converting to markdown: ", e$message,
              "\n for case: ", case_id, " for ", specialty, " (language: ", language, ")"))
  })
}