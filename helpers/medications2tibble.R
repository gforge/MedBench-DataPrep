#' Convert medications data from Excel file to tibble format
#'
#' This function reads an Excel file containing medications data and converts it into a tibble format.
#' It performs various data cleaning and transformation steps to ensure the data is in the desired format.
#'
#' @param fn The filename of the Excel file.
#' @param language The language of the column names in the Excel file.
#'
#' @return A tibble containing the medications data in the desired format.
#'
#' @examples
#' medications2tibble("medications.xlsx", "Swedish")
medications2tibble <- function(fn, language) {
  path <- glue("data/Cases/{fn}")

  sheets <- readxl::excel_sheets(path)
  sheet <- keep(sheets, \(x) startsWith(x, "Med") | startsWith(x, "Läkemedel"))
  if (length(sheet) != 1) {
    stop("No 'Medications' sheet found in file ", fn, "\n Got sheets: ", sheets |> paste(collapse = ", "), "\n expected sheet: Medications")
  }

  raw_data <-  suppressMessages({
    path |>
      readxl::read_excel(sheet = sheet,
                         trim_ws = TRUE)
  })
  if ("Way of adminstration" %in% colnames(raw_data)) {
    raw_data <- rename_with(raw_data, \(x) "Way of administration", .cols = matches("Way of adminstration"))
  }

  if (!("ATC-code" %in% colnames(raw_data))) {
    if ("ATC code" %in% colnames(raw_data)) {
      raw_data <- rename_with(raw_data, \(x) "ATC-code", .cols = matches("ATC code"))
    } else {
      warning("Missing ATC-column in Medications sheet for file: ", fn)
      raw_data <- raw_data |>
        mutate(`ATC-code` = '-')
    }
  }

  if (!("Medication" %in% colnames(raw_data))) {
    alt_name_w_lang <- switch(language,
                              "Swedish" = "Läkemedel (SE)",
                              "original" = "Medication (EN)",
                              "Medication (EN)")
    if (alt_name_w_lang %in% colnames(raw_data)) {
      raw_data <- rename_with(raw_data, \(x) "Medication", .cols = one_of(alt_name_w_lang))
    } else {
      stop("Missing columns in Medications sheet for file: ", fn,
           "\n Got columns: ", colnames(raw_data) |> paste(collapse = ", "),
           "\n expected columns: Medication")
    }
  }

  # Select columns with medication information and date YYYY-MM-DD or Excel numeric
  core_data <-  raw_data |>
    select(`Medication`, `Way of administration`, Strength, Unit, `ATC-code`, matches("20[0-2]\\d-[0-1]\\d-[0-3]\\d|^\\d+$")) |>
    tidyr::pivot_longer(cols = !`Medication` & !`Way of administration` & !Strength & !Unit & !`ATC-code`,
                        names_to = "Date",
                        values_to = "Times_per_day",
                        values_drop_na = TRUE) |>
    # Convert excel date, e.g. 44928, to R timestamp
    dplyr::mutate(parsed_date = suppressWarnings(dplyr::case_when(str_detect(Date, "^20[0-2]\\d-[0-1]\\d-[0-3]\\d \\d\\d:\\d\\d(:\\d\\d|)$") ~ lubridate::ymd(str_replace(Date, " .*", "")),
                                                                  str_detect(Date, "^20[0-2]\\d-[0-1]\\d-[0-3]\\d") ~ lubridate::ymd(Date),
                                                                  TRUE ~ as.Date(as.numeric(Date), origin = "1899-12-30"))),
                  timestamp = lubridate::ymd_hm(paste(parsed_date, "00:00")))

  bad_dates <- core_data |>
    filter(is.na(parsed_date))
  if (nrow(bad_dates)) {
    stop("Invalid dates in file ", fn, " found:\n -", bad_dates$Date |>
           paste(collapse = "\n -"))
  }

  core_data |>
    select(-Date) |>
    rename(date = parsed_date)
}
