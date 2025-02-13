# Load libraries
library(testthat)
library(dplyr)

# Sample data to simulate network connections
test_data <- tibble::tibble(
  node_id = c("A", "B", "C", "A", "C", "D"),
  connection_id = c("X", "Y", "X", "Z", "Z", "Y"),
  node_number = c(1, 2, 3, 1, 3, 4),
  weight = c(2.5, 3.0, 1.5, 2.0, 2.5, 3.5),
  from_to = c("from", "to", "from", "to", "from", "to")
  )

# Define a simple weight function for aggregation
weight_mean <- function(x) mean(x, na.rm = TRUE)

test_that("make_edges() returns expected structure without weight", {
  result <- make_edges(
    connections_data = test_data,
    node_id = node_id,
    node_number = node_number,
    connection_id = connection_id
    )

  expect_s3_class(result, "data.frame") # Ensure output is a df
  expect_true(all(c("from", "to", "weight") %in% colnames(result))) # Check expected columns
  expect_true(nrow(result) > 0) # Ensure edges were created
  })

test_that("make_edges() correctly calculates weight", {
  result <- make_edges(
    connections_data = test_data,
    node_id = node_id,
    node_number = node_number,
    connection_id = connection_id,
    weight_col = weight,
    weight_col_fun = weight_mean
    )

  expect_true("weight" %in% colnames(result)) # Ensure weight column exists
  expect_type(result$weight, "double") # Ensure weight is numeric
  })

test_that("make_edges() throws an error when weight_col is used without weight_col_fun", {
  expect_error(
    make_edges(
      connections_data = test_data,
      node_id = node_id,
      node_number = node_number,
      connection_id = connection_id,
      weight_col = weight
      ),
    "You must use `weight_col` and `weight_col_fun` in combination."
    )
  })

test_that("make_edges() correctly filters based on from_to_col", {
  result <- make_edges(
    connections_data = test_data,
    node_id = node_id,
    node_number = node_number,
    connection_id = connection_id,
    from_to_col = from_to
    )

  expect_true(nrow(result) > 0) # Ensure some edges remain
  expect_false(any(result$from == result$to)) # Ensure no self-loops
  })

test_that("make_edges() throws an error for invalid from_to_col values", {
  bad_data <- test_data
  bad_data$from_to <- c("from", "to", "middle", "to", "from", "to") # Introduce an invalid value

  error_message <- tryCatch(
    make_edges(
      connections_data = bad_data,
      node_id = node_id,
      node_number = node_number,
      connection_id = connection_id,
      from_to_col = from_to
      ),
    error = function(e) {
      print(e$message)  # Print the actual error message
      return(e$message) # Return it so we can inspect
    }
  )

  expect_match(error_message, "Only valid values for `from_to_col` are 'from' and 'to' \\(case sensitive\\).")
})

