test_that("flagweirdanimals flags weird values correctly", {
  data <- data.frame(animal = c("dog", "cat", "unicorn", "dragon"))

  # Test for flagging correctness
  result <- flagweirdanimals(data, column = "animal", valid_types = c("dog", "cat"))
  expect_equal(result$weird, c(FALSE, FALSE, TRUE, TRUE))
})

test_that("flagweirdanimals handles empty data", {
  data <- data.frame(animal = character(0))

  # Test for empty output
  result <- flagweirdanimals(data, column = "animal")
  expect_equal(nrow(result), 0)
})

test_that("flagweirdanimals handles non-existent columns", {
  data <- data.frame(animal = c("dog", "cat"))

  # Test for error on non-existent column
  expect_error(flagweirdanimals(data, column = "nonexistent"))
})
