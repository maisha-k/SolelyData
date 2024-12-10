test_that("Mostly numeric column is flagged correctly", {
  df <- data.frame(a = c(1, 2, "x", 4, NA), b = c("a", "b", "c", "d", "e"))
  result <- flagnonnumeric(df, "a", threshold = 0.8, replace_with_na = TRUE)

  # Check that the non-numeric value is flagged correctly
  expect_equal(result$non_numeric_values, "x")

  # Check that the non-numeric value is replaced with NA
  expect_true(is.na(result$data$a[3]))
})

