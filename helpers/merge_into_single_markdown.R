#' Merge the data into a single markdown file
#' 
#' This function takes in a list of data frames and merges them into a single markdown file.
#' 
#' @param data The list of data frames to be merged
#' @return The merged data in markdown format
#' @export
merge_into_single_markdown <- local({
  process_word_data <- function(notes) {
    notes |>
      mutate(content = glue("# {type}, {date}, {time}, {author}\n{content}"),
             timestamp = ymd_hm(paste(date, time)))
  }
  
  process_medications_data <- function(medications) {
    medications |>
      group_by(date) |>
      # Build a bullet list of the medications
      summarise(content = glue(" * {medication}, {`wayOfAdministration`}, {strength} {unit} {timesPerDay} times per day") |>
                  paste(collapse = "\n"),
                .groups = "drop") |>
      mutate(content = glue("# Medications, {date}\n{content}"),
             timestamp = ymd_hm(paste(date, "00:00")))
  }
  
  process_lab_values_data <- function(lab) {
    if (is.null(lab)) return(NULL)
    
    lab |>
      mutate(date = map_chr(date, toString),
             timestamp = paste(date, time),
             timestamp = ymd_hm(timestamp)) |> 
      group_by(timestamp) |>
      # Build a bullet list of the lab values
      summarise(content = paste0(" * ", labTest, " ", value, " ", unit,
                                 glue(" ({`referenceInterval`})"),
                                 collapse = "\n"),
                .groups = "drop") |>
      mutate(content = glue("# Lab values, {strftime(timestamp, '%Y-%m-%d, %H:%M')}\n{content}"))
  }
  
  function(data) {
    tryCatch({
      word_data <- process_word_data(data$notes)
      medications_data <- process_medications_data(data$medications)
      lab_values_data <- process_lab_values_data(data$lab)
    }, 
    warning = function(w) {
      print(w)
      browser()
    },
    error = function(e) {
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
})