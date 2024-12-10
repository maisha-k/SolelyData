#' Identify Solo Typos in a Data Frame Column
#'
#' This function identifies potential typos in a specified column of a data frame
#' based on string similarity. It suggests corrections for values that occur
#' only once without making any changes to the data.
#'
#' @param data A data frame.
#' @param column_name A character string specifying the column to check for typos.
#' @param threshold A numeric value between 0 and 1 indicating the maximum string distance
#'                  (Jaro-Winkler similarity) to consider for possible matches. Default is 0.2.
#' @return A list of typos and their suggested corrections.
#' @examples
#' df <- data.frame(a = c("apple", "apple", "appl", "orange", "banana",
#' "banana", "banan"), stringsAsFactors = FALSE)
#' typos <- solotypo(df, "a", threshold = 0.15)
#' print(typos)
#' @export
solotypo <- function(data, column_name, threshold = 0.2) {
  # Load the required library
  if (!requireNamespace("stringdist", quietly = TRUE)) {
    stop("The 'stringdist' package is required but not installed. Install it using install.packages('stringdist').")
  }

  # Validate the threshold
  if (!is.numeric(threshold) || threshold <= 0 || threshold > 1) {
    stop("Threshold must be a numeric value between 0 and 1.")
  }

  # Check if the column exists
  if (!column_name %in% colnames(data)) {
    stop(sprintf("Column '%s' does not exist in the data.", column_name))
  }

  # Ensure the column is character
  if (!is.character(data[[column_name]])) {
    stop(sprintf("Column '%s' must be of type character.", column_name))
  }

  # Convert column values to lowercase
  col_values <- tolower(data[[column_name]])

  # Calculate frequencies
  value_counts <- table(col_values)
  single_occurrences <- names(value_counts[value_counts == 1])

  # Initialize a list to store typos and suggestions
  typo_suggestions <- list()

  # Proceed if all other values appear more than once
  if (all(value_counts[!names(value_counts) %in% single_occurrences] > 1)) {
    for (value in single_occurrences) {
      # Find the closest match
      other_values <- names(value_counts)[names(value_counts) != value]
      distances <- stringdist::stringdist(value, other_values, method = "jw")
      closest_match <- other_values[which.min(distances)]

      # Check if the closest match meets the threshold
      if (min(distances) <= threshold) {
        # Add the typo and suggestion to the list
        typo_suggestions[[value]] <- closest_match
      }
    }
  } else {
    message("Not all other values appear more than once. No suggestions made.")
  }

  # Return the list of typos and suggestions
  return(typo_suggestions)
}

