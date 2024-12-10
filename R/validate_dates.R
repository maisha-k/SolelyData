#' Validate Dates in Dataset Columns
#'
#' This function checks whether specified columns in a dataset contain valid dates.
#' Dates are validated based on year, month, day, and optional constraints on year ranges.
#'
#' @param data A data frame containing the dataset.
#' @param date_columns A character vector specifying the column names to validate as dates.
#' @param min_year Optional. The minimum permissible year for validation (default is NA).
#' @param max_year Optional. The maximum permissible year for validation (default is NA).
#' @return A data frame summarizing the validity of dates in each column, including
#'         the count of invalid dates and their indices.
#' @examples
#' # example code
#' df <- data.frame(
#'date1 = c("2023-01-01", "2023-02-30", "2023-03-15", "not-a-date"),
#'date2 = c("2022-12-31", "2023-13-01", "2023-03-10", "2023-02-28"))
#'
#'results <- validate_dates(df, date_columns = c("date1", "date2"), min_year = 2020, max_year = 2025)
#'print(results)
#'print(attr(results, "invalid_indices"))
#' @export
validate_dates <- function(data, date_columns, min_year = NA, max_year = NA) {
  # Ensure input is a data frame
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }

  # Ensure date_columns are present in the data
  if (!all(date_columns %in% names(data))) {
    stop("Some specified columns are not present in the dataset.")
  }

  # Function to validate a single column
  validate_column <- function(col_data, min_year, max_year) {
    # Split the column into year, month, and day
    components <- strsplit(as.character(col_data), "-")
    components <- lapply(components, function(x) {
      if (length(x) != 3) return(c(NA, NA, NA))
      x
    })
    y <- as.numeric(sapply(components, `[`, 1))
    m <- as.numeric(sapply(components, `[`, 2))
    d <- as.numeric(sapply(components, `[`, 3))

    # Validate using checkdate logic
    is_valid <- !is.na(y) &
      (is.na(min_year) | y >= min_year) &
      (is.na(max_year) | y <= max_year) &
      !is.na(m) & m > 0 & m <= 12 &
      !is.na(d) & d > 0 & d <= 31 &
      suppressWarnings(!is.na(as.Date(paste(y, m, d, sep = "-"), format = "%Y-%m-%d")))

    list(
      valid = is_valid,
      invalid_indices = which(!is_valid)
    )
  }

  # Validate each specified column
  results <- lapply(date_columns, function(col) {
    validation <- suppressWarnings(validate_column(data[[col]], min_year, max_year))
    list(
      column = col,
      total_invalid = length(validation$invalid_indices),
      invalid_indices = validation$invalid_indices
    )
  })

  # Convert results to a data frame
  results_df <- data.frame(
    column = sapply(results, `[[`, "column"),
    total_invalid = sapply(results, `[[`, "total_invalid"),
    stringsAsFactors = FALSE
  )

  # Attach invalid indices as an attribute
  attr(results_df, "invalid_indices") <- lapply(results, `[[`, "invalid_indices")

  return(results_df)
}

