#'  Missing Data Summary
#'
#' This function provides a summary of missing data for each column in a data frame.
#' It identifies missing values based on `NA`, a user-specified set of placeholders
#' (e.g., `""`, `"N/A"`, `"Missing"` for characters or `-999`, `0` for numerics),
#' and returns a nicely formatted table.
#'
#' @param data A data frame to analyze
#' @param missing_values A vector of additional values to treat as missing.
#'   Can include both character and numeric values. Defaults to `c("", "NA", "N/A", "Missing")`.
#'
#' @return A nicely formatted table summarizing the missing data in the data frame.
#'   Includes the column names, count of missing values, and percentage of missing values.
#'
#'
#' @examples
#' library(dplyr)
#' example_data <- data.frame(
#'   A = c(1, 2, NA, 4, ""),
#'   B = c("apple", "N/A", "banana", NA, "Missing"),
#'   C = c(10, 20, 30, 40, 50)
#' )
#' #using user-specified missing values
#' missingsum(example_data, missing_values = c("", "N/A", "Unknown"))
#'
#' #using default missing values
#' missingsum(example_data)
#' @export

missingsum <- function(data, missing_values = c("", "NA", "N/A", "Missing")) {
  # Ensure 'data' is a data frame
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }

  # Validate 'missing_values' input
  if (!is.vector(missing_values)) {
    stop("The 'missing_values' parameter must be a vector.")
  }

  # Handle empty data frames
  if (nrow(data) == 0 || ncol(data) == 0) {
    return(data.frame(Column = character(0), MissingCount = integer(0), MissingPercent = numeric(0)))
  }

  # Helper function to identify missing values
  is_missing <- function(x) {
    if (is.factor(x)) x <- as.character(x) # Convert factors to character for consistency
    if (is.character(x)) {
      is.na(x) | x %in% missing_values
    } else {
      is.na(x)
    }
  }

  # Calculate missing data summary
  summary <- data |>
    dplyr::summarise(dplyr::across(dplyr::everything(), ~ sum(is_missing(.)), .names = "MissingCount_{col}")) |>
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "Column", values_to = "MissingCount") |>
    dplyr::mutate(
      MissingPercent = (MissingCount / nrow(data)) * 100
    )

  # Check if gt is installed
  if (requireNamespace("gt", quietly = TRUE)) {
    # Create a nicely formatted table using gt
    gt_table <- summary |>
      gt::gt() |>
      gt::tab_header(
        title = "Missing Data Summary",
        subtitle = "Analysis of missing values by column"
      ) |>
      gt::fmt_number(
        columns = "MissingPercent",
        decimals = 2
      ) |>
      gt::cols_label(
        Column = "Column Name",
        MissingCount = "Missing Count",
        MissingPercent = "Missing Percentage (%)"
      ) |>
      gt::tab_style(
        style = list(gt::cell_text(weight = "bold")),
        locations = gt::cells_column_labels()
      ) |>
      gt::opt_align_table_header(align = "center")

    return(gt_table)
  } else {
    warning("The 'gt' package is not installed. Returning a raw summary table.")
    return(summary)
  }
}
