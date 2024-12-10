#' Flag Weird Values
#'
#' This function flags rows in a dataset where values in a specified column
#' are not included in a predefined list of valid values. While it is designed
#' with general data in mind, it can be particularly useful for identifying unusual
#' entries in any context, including but not limited to animal types.
#'
#' @param data A data frame containing the dataset to analyze.
#' @param column A string specifying the column name in the dataset to validate.
#' @param valid_types A character vector of valid values for the specified column. Defaults to `c("dog", "cat", "rabbit")`.
#' @return A modified data frame with an additional column `weird`, which is `TRUE` for rows with values not in `valid_types` and `FALSE` otherwise.
#' @examples
#' # Example dataset for shelter data
#' shelter_data <- data.frame(
#'   animal_id = 1:5,
#'   type = c("Dog", "Cat", "Hamster", "Rabbit", "Lizard")
#' )
#'
#' # Flag weird animal types
#' result <- flagweirdanimals(shelter_data, column = "type",
#' valid_types = c("dog", "cat", "rabbit"))
#' print(result)
#'
#' # Example dataset for general use
#' fruit_data <- data.frame(
#'   fruit_id = 1:5,
#'   fruit_type = c("Apple", "Banana", "Durian", "Grape", "Mango")
#' )
#'
#' # Flag weird fruit types
#' result <- flagweirdanimals(fruit_data, column = "fruit_type",
#'  valid_types = c("apple", "banana", "grape"))
#' print(result)
#'
#' @export
flagweirdanimals <- function(data, column, valid_types = c("dog", "cat", "rabbit")) {
  # Ensure the column exists in the dataset
  if (!column %in% names(data)) {
    stop(paste("The specified column", column, "does not exist in the dataset."))
  }

  # Standardize the column values and valid_types to lowercase for comparison
  data$weird <- !tolower(data[[column]]) %in% tolower(valid_types)

  return(data)
}
