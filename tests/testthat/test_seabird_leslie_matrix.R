test_that("get_seabird_matrix return a 3x3 matrix", {
  expected_matrix <- matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 1), nrow = 3, byrow = TRUE)
  obtained_matrix <- get_seabird_matrix(c(0, 0, 0), c(0, 0), 1)
  expect_equal(expected_matrix, obtained_matrix)
})
