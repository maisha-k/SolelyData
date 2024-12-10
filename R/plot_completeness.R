#' Plot Completeness of Dataset Rows
#'
#' This function generates a histogram showing the number of non-missing values (neither `NA` nor `""`)
#' per row in a dataset. It provides an overview of data completeness for each row.
#'
#' @param data A data frame to analyze.
#' @param title A character string specifying the title of the histogram. Defaults to `"Row Completeness"`.
#' @return A ggplot2 object displaying the histogram of completeness.
#' @examples
#' # Example dataset
#' example_data <- data.frame(
#'   A = c(1, NA, 3, 4, ""),
#'   B = c("a", "b", NA, "", "e"),
#'   C = c(NA, 2, 3, 4, 5)
#' )
#'
#' # Generate the completeness plot
#' plot_completeness(example_data)
#'
#' @export
plot_completeness <- function(data, title = "Row Completeness") {
  # Ensure the input is a data frame
  if (!is.data.frame(data)) {
    stop("The input must be a data frame.")
  }

  if (nrow(data) == 0 || ncol(data) == 0) {
    stop("The data frame is empty and cannot be visualized.")
  }

  total_columns <- ncol(data)

  # Calculate non-missing values per row
  completeness <- apply(data, 1, function(row) {
    sum(!is.na(row) & row != "")
  })

  # Create a data frame for plotting
  completeness_data <- data.frame(
    row_index = seq_along(completeness),
    non_missing_values = completeness
  )

  full_title <- paste(title, "(Total Columns:", total_columns, ")")

  # Generate the histogram with explicit ggplot2 calls
  plot <- ggplot2::ggplot(completeness_data, ggplot2::aes(x = non_missing_values)) +
    ggplot2::geom_histogram(binwidth = 1, fill = "blue", alpha = 0.7, color = "black") +
    ggplot2::labs(
      title = full_title,
      x = "Number of Non-Missing Values per Row",
      y = "Count of Rows"
    ) +
    ggplot2::theme_minimal()

  return(plot)

}
