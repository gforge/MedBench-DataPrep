build_chart_from_JSON_data <- function(chart, 
                                       dataFromFiles,
                                       colnames4chart) {
  tryCatch({
    raw_data <- dataFromFiles |> 
      detect(\(c) attr(c, "specialty") == chart$specialty & attr(c, "language") == chart$language & attr(c, "case_id") == chart$name)
    if (!is.null(raw_data)) {
      ret <- raw_data
    } else {
      ret <- list()
    }
    
    hasData <- function(x) {
      return(!is.null(x) && length(x) > 0)
    }

    ret$notes <- chart$notes |> 
      map(as_tibble) |> 
      bind_rows() |> 
      extract(colnames4chart$noteColumns)

    if (hasData(chart$medications)) {
      ret$medications <- chart$medications |> 
        map(as_tibble) |> 
        bind_rows() |> 
        extract(colnames4chart$medColumns)
    }
    
    if (hasData(chart$labValues)) {
      ret$lab <- chart$labValues |> 
        map(as_tibble) |> 
        bind_rows() |> 
        extract(colnames4chart$labColumns)
    }
    
    if (hasData(chart$summaries)) {
      ret$summaries <- chart$summaries |> 
        map(as_tibble) |> 
        bind_rows() |> 
        mutate(specialty = chart$specialty, language=chart$language, name = chart$caseId) |> 
        extract(colnames4chart$summaryColumns) |>
        distinct() # Remove duplicates as each review has its own summary (although identical)
    }
  }, error = function(e) {
    print(e)
    # There is a bug in one of the json objects
    browser()
    exit()
  })

  structure(ret, 
            class = c("chartData", class(ret)),
            specialty = chart$specialty,
            language = chart$language,
            case_id = chart$name)
}

extend_with_platform_JSON <- function(originalData, jsonPath  = "data/output/allData.json") {
  if (!file.exists(jsonPath)) {
    stop("File does not exist: ", jsonPath)
  }
  
  jsonData <- read_file(file = jsonPath) |> 
    jsonlite::fromJSON(simplifyVector = FALSE)
  
  colnames4chart <- originalData |> 
    map(\(x) list(noteColumns = colnames(x$notes),
                  labColumns = colnames(x$lab),
                  medColumns = colnames(x$med),
                  medicationColumns = colnames(x$medications),
                  summaryColumns = colnames(x$summaries))) |> 
    reduce(\(a, b) {
      for (name in union(names(a), names(b))) {
        a[[name]] <- union(a[[name]], b[[name]])
      }
      return(a)
    })
  
  allCharts <- jsonData |> 
    map(\(c) c$charts) |>
    list_flatten()
  
  updatedCharts <- map(allCharts,
                       \(c) build_chart_from_JSON_data(c, 
                                                       colnames4chart = colnames4chart, 
                                                       dataFromFiles = originalData))
  
  chartsNotInUpdatedData <- originalData |> 
    keep(\(org) detect(updatedCharts, 
                          \(updated) attr(updated, "case_id") == attr(org, "case_id") & 
                            attr(updated, "specialty") == attr(org, "specialty") & 
                            attr(updated, "language") == attr(org, "language")) |> 
              is.null())
  
  if (length(chartsNotInUpdatedData) > 0) {
    updatedCharts <- c(updatedCharts, chartsNotInUpdatedData)
  }
  
  structure(updatedCharts,
            class = c("chartDataList", class(updatedCharts)),
            noJsonCharts = length(allCharts),
            notOnServer = length(chartsNotInUpdatedData))
}

print.chartData <- function(x) {
  cat("Specialty: ", attr(x, "specialty"), 
      " > ", attr(x, "case_id"),
      " > ", attr(x, "language"), 
      "\n",
      sep = "")
  
  cat(" Notes: ", length(x$notes), "\n")
  cat(" Medications: ", length(x$medications), "\n")
  cat(" Lab Values: ", length(x$lab), "\n")
  
  if (exists("x$summaries")) {
    cat(" Summaries: ", length(x$summaries), "\n")
  }
  invisible(x)
}

print.chartDataList <- function(x) {
  cat("Number of charts: ", length(x), 
      " (There are ", attr(x, "noJsonCharts"), " json charts",
      ", of which ", attr(x, "notOnServer"), " aren't already on the server)\n",
      sep = "")
  invisible(x)
}


