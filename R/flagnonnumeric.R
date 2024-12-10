#' Flag and Handle Non-Numeric Values in a Mostly Numeric Column
#'
#' This function checks if a specified column in a data frame is mostly numeric
#' based on a user-defined threshold (default is 0.95). If non-numeric values
#' are present, it flags them and optionally replaces them with NA.
#'
#' @param data A data frame.
#' @param column_name A character string specifying the column to check.
#' @param threshold A numeric value between 0 and 1 indicating the proportion
#'                  of numeric or NA values required to consider the column
#'                  "mostly numeric." Default is 0.95.
#' @param replace_with_na Logical, whether to replace non-numeric values with NA. Default is TRUE.
#'
#' @return A list with two elements:
#'         - `data`: The modified data frame (if `replace_with_na = TRUE`).
#'         - `non_numeric_values`: A vector of the non-numeric values flagged.
#'
#' @examples
#' df <- data.frame(a = c(1, 2, "x", 4, NA), b = c("a", "b", "c", "d", "e"))
#' result <- flagnonnumeric(df, "a", threshold = 0.8, replace_with_na = TRUE)
#' print(result$data)
#' print(result$non_numeric_values)
#' @export
flagnonnumeric <- function(data, column_name, threshold = 0.95, replace_with_na = TRUE) {
  # Validate the threshold
  if (!is.numeric(threshold) || threshold <= 0 || threshold > 1) {
    stop("Threshold must be a numeric value between 0 and 1.")
  }

  # Check if the column exists
  if (!column_name %in% colnames(data)) {
    stop(sprintf("Column '%s' does not exist in the data.", column_name))
  }

  # Extract the column
  col_values <- data[[column_name]]

  # Check for numeric or NA values
  is_numeric <- suppressWarnings(!is.na(as.numeric(as.character(col_values))))
  numeric_or_na <- sum(is.na(col_values) | is_numeric, na.rm = TRUE)
  total_values <- length(col_values)

  # Initialize a list to store non-numeric values
  non_numeric_values <- col_values[!is.na(col_values) & !is_numeric]

  # Check if the column meets the threshold for being "mostly numeric"
  if (numeric_or_na / total_values >= threshold) {
    message(sprintf("Column '%s' is mostly numeric (%.1f%% numeric or NA).", column_name, (numeric_or_na / total_values) * 100))

    if (length(non_numeric_values) > 0) {
      message("Flagging the following non-numeric values:")
      print(unique(non_numeric_values))

      # Optionally replace non-numeric values with NA
      if (replace_with_na) {
        col_values[!is.na(col_values) & !is_numeric] <- NA
        data[[column_name]] <- col_values
      }
    }
  } else {
    message(sprintf("Column '%s' is not mostly numeric (%.1f%% numeric or NA). No changes made.", column_name, (numeric_or_na / total_values) * 100))
  }

  # Return the modified data and flagged non-numeric values
  return(list(
    data = data,
    non_numeric_values = unique(non_numeric_values)
  ))
}

