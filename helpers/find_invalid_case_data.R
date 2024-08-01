#' Find invalid case data
#'
#' This function is used to find invalid case data in a single case.
#'
#' @param single_case A single case to check for invalid data.
#' @return A vector of errors found in the case data.
find_invalid_case_data <- function(single_case) {
  errors <- c()
  date_range <- single_case$notes$date |> unique() |> as.Date() |> range()

  duplicated_notes <- single_case$notes |>
    group_by(date, time, author) |>
    mutate(no = row_number(), n = n()) |>
    filter(n > 1) |>
    select(-no, -n)
  if (nrow(duplicated_notes) > 0) {
    errors <- append(errors,
                     glue("Duplicated notes for case {case_id} for {specialty} ({language})",
                          " on {dates} at {times} by {authors}",
                          dates = duplicated_notes$date |> as.character() |> paste(collapse = ", "),
                          times = duplicated_notes$time |> as.character() |> paste(collapse = ", "),
                          authors = duplicated_notes$author |> as.character() |> paste(collapse = ", "),
                          case_id = attr(single_case, "case_id"),
                          specialty = attr(single_case, "specialty"),
                          language = attr(single_case, "language")))
  }

  if (!is.null(single_case$lab)) {
    lab_outside <- single_case$lab |>
      filter(date < date_range[1] - 1 | date > date_range[2] + 1)
    if (nrow(lab_outside) > 0) {
      errors <- append(errors,
                       glue("Lab values for case {case_id} for {specialty} ({language})",
                            " are outside of chart date range: {dates} for the range {start} -> {end}",
                            dates = lab_outside$date |> unique() |> as.character() |> paste(collapse = ", "),
                            start = date_range[1],
                            end = date_range[2],
                            case_id = attr(single_case, "case_id"),
                            specialty = attr(single_case, "specialty"),
                            language = attr(single_case, "language")))
    }

    no_unit <- single_case$lab |>
      filter(is.na(unit) | unit == "") |>
      distinct(labTest)
    if (nrow(no_unit) > 0) {
      errors <- append(errors,
                       glue("Lab values for case {case_id} for {specialty} ({language})",
                            " have missing units for: {tests}",
                            tests = no_unit$labTest |> as.character() |> paste(collapse = ", "),
                            case_id = attr(single_case, "case_id"),
                            specialty = attr(single_case, "specialty"),
                            language = attr(single_case, "language")))
    }
  }

  if (!is.null(single_case$medications)) {
    med_outside <- single_case$medications |>
      filter(date < date_range[1] - 1 | date > date_range[2] + 1)
    if (nrow(med_outside) > 0) {
      errors <- append(errors,
                       glue("Medication valuesfor case {case_id} for {specialty} ({language})",
                            " are outside of chart date range: {dates} for the range {start} -> {end}",
                            dates = med_outside$date |> unique() |> as.character() |> paste(collapse = ", "),
                            start = date_range[1],
                            end = date_range[2],
                            case_id = attr(single_case, "case_id"),
                            specialty = attr(single_case, "specialty"),
                            language = attr(single_case, "language")))
    }

    meds_without_strength <- single_case$medications |>
      filter(is.na(strength) | strength == "") |>
      distinct(medication)
    if (nrow(meds_without_strength) > 0) {
      errors <- append(errors,
                       glue("Medication values for case {case_id} for {specialty} ({language})",
                            " have missing strength for: {meds}",
                            meds = meds_without_strength$medication |> as.character() |> paste(collapse = ", "),
                            case_id = attr(single_case, "case_id"),
                            specialty = attr(single_case, "specialty"),
                            language = attr(single_case, "language")))
    }

    meds_without_unit <- single_case$medications |>
      filter(is.na(unit) | unit == "") |>
      distinct(medication)
    if (nrow(meds_without_unit) > 0) {
      errors <- append(errors,
                       glue("Medication values for case {case_id} for {specialty} ({language})",
                            " have missing unit for: {meds}",
                            meds = meds_without_unit$medication |> as.character() |> paste(collapse = ", "),
                            case_id = attr(single_case, "case_id"),
                            specialty = attr(single_case, "specialty"),
                            language = attr(single_case, "language")))
    }

    meds_without_way_of_admin <- single_case$medications |>
      filter(is.na(wayOfAdministration) | wayOfAdministration == "") |>
      distinct(medication)
    if (nrow(meds_without_way_of_admin) > 0) {
      errors <- append(errors,
                       glue("Medication values for case {case_id} for {specialty} ({language})",
                            " have missing way of administration for: {meds}",
                            meds = meds_without_way_of_admin$medication |> as.character() |> paste(collapse = ", "),
                            case_id = attr(single_case, "case_id"),
                            specialty = attr(single_case, "specialty"),
                            language = attr(single_case, "language")))
    }

    no_medication_days <- single_case$medications |>
      distinct(date) |>
      nrow()
    no_days <- as.numeric(date_range[2] - date_range[1])
    if (no_medication_days - no_days < 0) {
      errors <- append(errors,
                       glue("Expected one medication per day for case {case_id} for {specialty} ({language})",
                            ", got only: {dates} for the range {start} -> {end}",
                            dates = single_case$medications$date |> unique() |> as.character() |> paste(collapse = ", "),
                            start = date_range[1],
                            end = date_range[2],
                            case_id = attr(single_case, "case_id"),
                            specialty = attr(single_case, "specialty"),
                            language = attr(single_case, "language")))
    }
  }

  return(errors)
}
