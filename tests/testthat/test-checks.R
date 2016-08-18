context("checks")

test_that("add_checks fails if an undefined type", {
  f1 <- function(blah = ? numeric) blah + 1
  expect_error(add_checks(f1), "'numeric' is an undefined type")

  f2 <- function(blah = 1 ? numeric) blah + 1
  expect_error(add_checks(f1), "'numeric' is an undefined type")
})

test_that("add_checks adds a check if a defined type", {
  spec("numeric", check = function(x) is.numeric(x))
  f1 <- function(blah = ? numeric) blah + 1
  expect_error(add_checks(f1), "'numeric' is an undefined type")

  f2 <- function(blah = 1 ? numeric) blah + 1
  expect_error(add_checks(f1), "'numeric' is an undefined type")
})
