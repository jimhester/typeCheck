context("checks")

test_that("add_checks fails if an undefined type", {
  f1 <- function(blah = ? numeric) blah + 1
  expect_error(add_checks(f1), "'numeric' is an undefined type")

  f2 <- function(blah = 1 ? numeric) blah + 1
  expect_error(add_checks(f1), "'numeric' is an undefined type")
})

test_that("add_checks adds a check if a defined type in formals", {
  spec("numeric", check = function(x) is.numeric(x))
  f1 <- function(blah = 1 ? numeric) blah
  f2 <- add_checks(f1)
  expect_error(f2(), NA)
  expect_error(f2(1), NA)
  expect_error(f2("character"), "`blah` is a `character` not a `numeric`.")

  f1 <- function(blah = ? numeric) blah
  f2 <- add_checks(f1)
  expect_error(f2(1), NA)
  expect_error(f2("character"), "`blah` is a `character` not a `numeric`.")

  f1 <- function(blah = ? numeric) { blah <- blah + 1 ; blah}
  f2 <- add_checks(f1)
  expect_error(f2(1), NA)
  expect_equal(f2(1), 2)
  expect_error(f2("character"), "`blah` is a `character` not a `numeric`.")

  f1 <- function(blah = ? numeric, foo) { blah }
  f2 <- add_checks(f1)
  expect_error(f2(1), NA)
  expect_error(f2("character"), "`blah` is a `character` not a `numeric`.")
})

test_that("add_checks adds a check if a defined type in body", {
  spec("numeric", check = function(x) is.numeric(x))
  f1 <- function(blah) blah ? numeric
  f2 <- add_checks(f1)
  expect_error(f2(1), NA)
  expect_error(f2("character"), "`blah` is a `character` not a `numeric`.")
})
