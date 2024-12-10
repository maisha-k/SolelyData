test_that("plot_completeness returns a ggplot object", {
  data <- data.frame(A = c(1, NA, 3), B = c("a", "", "b"))

  # Test for ggplot object
  plot <- plot_completeness(data)
  expect_s3_class(plot, "ggplot")
})

test_that("plot_completeness handles empty data", {
  data <- data.frame()

  # Test for expected error
  expect_error(plot_completeness(data), "The data frame is empty and cannot be visualized.")
})

