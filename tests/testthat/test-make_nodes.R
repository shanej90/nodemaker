library(testthat)
library(dplyr)
library(tibble)

test_that("make_nodes() creates unique nodes with an ID column", {
  data <- tibble(
    node = c("A", "B", "C", "A"),
    attr1 = c(1, 2, 3, 4),
    attr2 = c("x", "y", "z", "w")
    )

  result <- make_nodes(data, node, attr1)

  expect_s3_class(result, "tbl_df")  # Should return a tibble
  expect_true("id" %in% colnames(result))  # ID column should exist
  expect_equal(nrow(result), 3)  # Duplicates should be removed
  expect_equal(result$node, c("A", "B", "C"))  # Unique nodes
  })

test_that("make_nodes() handles empty input", {
  data <- tibble(node = character(), attr1 = numeric())

  result <- make_nodes(data, node, attr1)

  expect_equal(nrow(result), 0)  # Should return an empty tibble
  expect_equal(colnames(result), c("id", "node", "attr1"))  # Columns should match
  })

test_that("make_nodes() works with additional attributes", {
  data <- tibble(
    node = c("X", "Y", "X", "Z"),
    attr1 = c(10, 20, 10, 30),
    attr2 = c("foo", "bar", "foo", "baz")
    )

  result <- make_nodes(data, node, attr1, attr2)

  expect_true(all(c("id", "node", "attr1", "attr2") %in% colnames(result)))
  expect_equal(nrow(result), 3)  # Should remove duplicate nodes
  })

test_that("make_nodes() preserves column order", {
  data <- tibble(
    node = c("N1", "N2"),
    attrA = c("a", "b"),
    attrB = c(100, 200)
    )

  result <- make_nodes(data, node, attrB, attrA)

  expect_equal(colnames(result), c("id", "node", "attrB", "attrA"))  # Column order should match
  })

test_that("make_nodes() errors if node_col is missing", {
  data <- tibble::tibble(
    attr1 = c(1, 2, 3),
    attr2 = c("a", "b", "c")
  )

  error_message <- tryCatch(
    {
      make_nodes(data, missing_col)  # This should trigger an error
      NULL  # This line won't be reached if an error occurs
    },
    error = function(e) {
      e$message  # Capture the error message
    }
  )

  expect_error(make_nodes(data, missing_col), "Column `missing_col` doesn't exist.")

  # Print the error message for debugging purposes
  print(error_message)
})

