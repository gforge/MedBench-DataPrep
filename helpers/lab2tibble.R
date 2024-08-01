#' Convert lab data from Excel file to tibble format
#'
#' This function takes an Excel file path and a language parameter as input and converts the lab data from the file into a tibble format.
#' The lab data should be stored in a sheet named "Lab tests" in the Excel file.
#' The function performs the following steps:
#' 1. Reads the Excel file and extracts the "Lab tests" sheet.
#' 2. Renames the column "Referece interval" to "Reference interval" if it exists.
#' 3. Drops columns with all missing values.
#' 4. Checks if the "Lab test" column exists, and if not, checks for alternative column names based on the language parameter.
#' 5. Checks if the required columns ("Lab test", "Reference interval", "Unit") exist in the lab data.
#' 6. If the lab data has only 3 columns, it returns NULL with a warning message.
#' 7. Selects the "Lab test", "Reference interval", "Unit" columns, and columns with a timestamp in the format "YYYY-MM-DD HH:MM" or Excel numeric format.
#' 8. Converts the decimal values with comma decimal separator to dot decimal separator.
#' 9. Converts the timestamp column to the appropriate date format.
#'
#' @param fn The file path of the Excel file containing the lab data.
#' @param language The language parameter to determine alternative column names.
#'
#' @return A tibble containing the converted lab data.
#'
#' @examples
#' lab2tibble("/path/to/lab_data.xlsx", "English")
#' lab2tibble("/path/to/lab_data.xlsx", "Swedish")
#'
#' @export
lab2tibble <- function(fn, language) {
  convert_excel_datetime <- function(x) {
    if (length(x) > 1) stop("Use rowwise() before calling this function")

    if (str_detect(x, "\\d+\\.\\d+")) {
      # Extract the date and time components
      x <- as.numeric(x)
      date_part <- floor(x)
      time_fractional_minutes <- ((x - date_part) * 24 * 60) |>
        round() |>
        as.integer()  # Convert fraction to minutes

      return(as_date(date_part, origin = "1899-12-30") + lubridate::minutes(time_fractional_minutes))
    }

    if (str_detect(x, "^(20|)\\d\\d-\\d\\d-\\d\\d")) {
      return(lubridate::ymd_hm(x))
    }

    stop("Invalid date provided: ", x)
  }

  path <- glue::glue("data/Cases/{fn}")
  sheets <- readxl::excel_sheets(path)
  sheet <- keep(sheets, startsWith, "Lab")
  if (length(sheet) != 1) {
    stop("No 'Lab tests' sheet found in file ", fn, "\n Got sheets: ", sheets |> paste(collapse = ", "), "\n expected sheet: Lab tests")
  }

  raw_data <- suppressMessages({
    path |>
      readxl::read_excel(sheet = sheet,
                         trim_ws = TRUE)
  })
  if ("Referece interval" %in% colnames(raw_data)) {
    raw_data <- rename_with(raw_data, \(x) "Reference interval", .cols = matches("Referece interval"))
  }

  raw_lab_data <- raw_data |>
    # Drop columns with all missing values
    select(where(\(x) !all(is.na(x))))

  if (!("Lab test" %in% colnames(raw_lab_data))) {
    alt_lab_test_name <- switch(language,
                                "Swedish" = "Laboratorietester (SE)",
                                "English" = "Lab test (EN)",
                                "Lab test (EN)")
    if (alt_lab_test_name %in% colnames(raw_lab_data)) {
      raw_lab_data <- rename_with(raw_lab_data, \(x) "Lab test", .cols = one_of(alt_lab_test_name))
    } else {
      stop("Missing column 'Lab test' in Lab tests sheet for file: ", fn,
           "\n Got columns: ", colnames(raw_lab_data) |> paste(collapse = ", "),
           "\n expected column: Lab test")
    }
  }

  if (!all(c("Lab test", "Reference interval", "Unit") %in% colnames(raw_lab_data))) {
    stop("Missing columns in Lab tests sheet for file: ", fn,
         "\n Got columns: ", colnames(raw_lab_data) |> paste(collapse = ", "),
         "\n expected columns: Lab test, Reference interval, Unit")
  }

  if (ncol(raw_lab_data) == 3) {
    warning("File ", fn, " does not contain any lab values")
    return(NULL)
  }

  # Select Lab test, Reference interval, Unit and all columns with a YYYY-MM-DD HH:MM or Excel numeric format
  raw_lab_data |>
    select(`Lab test`, `Reference interval`, Unit, matches("20\\d{2,2}\\-\\d{2,2}\\-\\d{2,2} \\d{2,2}:\\d{2,2}|^\\d+\\.\\d+$")) |>
    pivot_longer(cols = !`Lab test` & !`Reference interval` & !Unit ,
                 names_to = "timestamp",
                 values_to = "Value",
                 # Transform , decimal to . decimal
                 values_transform = list(Value = \(v)
                                         as.character(v) |>
                                           str_replace_all(",", ".") |>
                                           as.numeric()),
                 values_drop_na = TRUE) |>
    rowwise() |>
    mutate(timestamp = convert_excel_datetime(timestamp))
}
