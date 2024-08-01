#' Convert Word Document to Tibble
#'
#' This function takes a file path to a Word document and converts it into a tibble
#' with structured data. The Word document should follow a specific format where each
#' entry has a header line containing information such as date, hour, and author, followed
#' by the content of the entry.
#'
#' @param fn The file path to the Word document.
#'
#' @return A tibble with the following columns:
#'   - `header`: The header line of each entry.
#'   - `type`: The type of the entry (extracted from the header).
#'   - `year`: The year of the entry (extracted from the header).
#'   - `hour`: The hour of the entry (extracted from the header).
#'   - `author`: The author of the entry (extracted from the header).
#'   - `content`: The content of the entry.
#'   - `timestamp`: The timestamp of the entry, calculated from the year and hour.
word2tibble <- function(fn) {
  date_formats =  c(
    Iso = "20[0-2]\\d-[0-1]\\d-[0-3]\\d", # E.g. 2021-12-31
    European = "[0-3]\\d[-/][0-1]{0,1}\\d[-/]20[0-2]\\d" # E.g. 31-12-2021 or 31/12/2021
  )
  hour_formats = c(
    General = "[0-2]\\d[:.][0-6]\\d", # E.g. 23:59
    French = "[0-2]\\d[hH][0-6]\\d"
  )
  dr_names = c(
    general = "[Dd]r.{0,1} [^\n]+",
    Greek = "Δρ[^\n]+",
    Italian = "[Dd]ott[^\n]+"
  )

  regex_patterns <- list(
    date = paste(date_formats, collapse = "|"),
    hour = paste(hour_formats, collapse = "|"),
    author = paste(dr_names, collapse = "|")
  )

  regex_patterns$header <- glue("(?s)^(.+),[ ]+({date}),*[ ]+({hour}),[ ]+({author})",
                                date = regex_patterns$date,
                                hour = regex_patterns$hour,
                                author = regex_patterns$author) |>
    as.character()

  convertToIsoDateString <- function(date) {
    if (length(date) > 1) {
      return (map_chr(date, convertToIsoDateString))
    }

    # Remove leading and trailing white space
    date <- str_trim(date)

    if (str_detect(date, glue("^{date}$",
                              date = date_formats["European"]))) {
      return(lubridate::dmy(date) |> as.character())
    }

    if (str_detect(date, glue("^{date}$",
                              date = date_formats["Iso"]))) {
      return(date)
    }

    stop("Invalid date format: ", date)
  }

  convertToHourString <- function(hour) {
    if (length(hour) > 1) {
      return (map_chr(hour, convertToHourString))
    }

    hour <- str_trim(hour)

    if (str_detect(hour, glue("^{hour}$",
                              hour = hour_formats["General"]))) {
      return(str_replace(hour, "[:.]", ":"))
    }

    if (str_detect(hour, glue("^{hour}$",
                              hour = hour_formats["French"]))) {
      return(str_replace(hour, "[hH]", ":"))
    }

    stop("Invalid hour format: ", hour)
  }

  read_txt <- function(path) {
    if (!file.exists(path)) {
      stop("File ", path, " does not exist")
    }

    if (endsWith(path, ".docx")) {
      summary <- suppressWarnings(officer::read_docx(path) |>
                                    officer::docx_summary())
      return(summary$text)
    }

    return(readr::read_lines(path))
  }

  trim_empty_star_end_lines <- function(txt) {
    # Trim any empty starting and end lines
    while (str_detect(txt[1], "^[[:space:]]*$")) {
      txt <- txt[2:length(txt)]
    }
    while (str_detect(txt[length(txt)], "^[[:space:]]*$")) {
      txt <- txt[1:(length(txt) - 1)]
    }
    return(txt)
  }

  stip_any_accidental_pnrs <- function(txt) {
    # Convert 19121212-1212 and 1212121212 to 19121212 and 121212-xxxx
    txt |>
      str_replace_all("(\\d{6})-(\\d{4})", "\\1-xxxx") |>
      str_replace_all("(^|[^0-9])(\\d{8}|\\d{6})(\\d{4})", "\\1\\2-xxxx")
  }

  raw_text <- glue("data/Cases/{fn}") |>
    read_txt() |>
    trim_empty_star_end_lines() |>
    stip_any_accidental_pnrs()

  # Drop header from first line
  if (str_detect(raw_text[1], "^#", negate = TRUE)) {
    raw_text <- raw_text[2:length(raw_text)]
  }

  raw_text <- raw_text |> trim_empty_star_end_lines()

  raw_data <- raw_text |>
    paste(collapse = "\n") |>
    str_split(pattern = "(\n[ ]*|^)#[^#]") |>
    extract2(1) |>
    tibble(text = _) |>
    filter(nchar(text) > 0)

  base_data <- raw_data |>
    mutate(header = str_split(text, "\n") |> map_chr(\(x) x[[1]]),
           type = str_replace(header, regex_patterns$header, "\\1"),
           year = str_replace(header, regex_patterns$header, "\\2"),
           hour = str_replace(header, regex_patterns$header, "\\3"),
           author = str_replace(header, regex_patterns$header, "\\4"),
           content = str_replace(text, "^(.+[^\n])\n+(.+)", "\\2")) |>
    relocate(text, .after = -1)

  clean_data <- base_data |>
    mutate(year = convertToIsoDateString(year),
           hour = convertToHourString(hour),
           timestamp = lubridate::ymd_hm(paste(year, hour)))

  bad_headers <- clean_data |>
    filter(!str_detect(header, regex_patterns$header))
  if (nrow(bad_headers) > 0) {
    stop("In file ", fn, " found invalid headers:",
         "\n - ", bad_headers$header |>
           paste(collapse = "\n - "))
  }

  return(clean_data)
}