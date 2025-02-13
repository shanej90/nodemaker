# Load libraries
library(testthat)
library(dplyr)

# Load or create a test dataset
test_data <- iris

# Rename columns to simulate network data
colnames(test_data)[1:2] <- c("node", "connection")

test_that("make_connections() correctly processes data without weight", {
  result <- make_connections(test_data, node_col = node, connect_col = connection)

  expect_s3_class(result, "data.frame") # Check output is a tibble
  expect_true(all(c("node", "connection", "node_number") %in% colnames(result))) # Ensure expected columns exist
  expect_equal(nrow(result), nrow(test_data)) # Ensure all rows are retained
})

test_that("make_connections() correctly handles numeric weight column", {
  result <- make_connections(test_data, node_col = node, connect_col = connection, weight_col = Petal.Length)

  expect_true("Petal.Length" %in% colnames(result)) # Check if weight column is included
  expect_type(result$Petal.Length, "double") # Ensure weight column remains numeric
})

test_that("make_connections() throws an error when weight_col is non-numeric", {
  expect_error(
    make_connections(test_data, node_col = node, connect_col = connection, weight_col = Species),
    "`weight_col` must be numeric."
  )
})
