context("add_checks")
library(types)

test_that("functions without checks", {
  f <- function(x, y = 10) x + 10
  f2 <- add_checks(f)
  expect_identical(body(f2), body(f))
  expect_identical(formals(f2), formals(f))

  f <- function() { }
  f2 <- add_checks(f)
  expect_identical(body(f2), body(f))
  expect_identical(formals(f2), formals(f))

  f <- function(x = 10 + 10, y = 20 && FALSE) { x * y }
  f2 <- add_checks(f)
  expect_identical(body(f2), body(f))
  expect_identical(formals(f2), formals(f))
})

test_that("add_checks fails if an undefined type", {
  f1 <- function(blah = ? numeric) blah + 1
  expect_error(add_checks(f1), "'numeric' is an undefined type")

  f2 <- function(blah = 1 ? numeric) blah + 1
  expect_error(add_checks(f1), "'numeric' is an undefined type")
})

test_that("add_checks adds a check if a defined type in formals", {
  type("numeric", check = function(x) is.numeric(x))
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
  type("numeric", check = function(x) is.numeric(x))
  f1 <- function(blah) blah ? numeric
  f2 <- add_checks(f1)
  expect_error(f2(1), NA)
  expect_error(f2("character"), "`f1\\(\\)` is a `character` not a `numeric`.")
})

test_that("compound checks", {
  type("unary",
    check = function(x) length(x) == 1,
    error = function(n, v, t) sprintf("`%s` has length `%s`, not `1`", n, length(v)))
  type("numeric", check = function(x) is.numeric(x))
  type("equals_one",
    check = function(x) x == 1,
    error = function(n, v, t) sprintf("`%s` equals `%s`, not `1`", n, deparse(v)))
  f1 <- function(blah = ? unary) { blah ? numeric } ? equals_one
  f2 <- add_checks(f1)

  expect_error(f2(1:2), "`blah` has length `2`, not `1`")
  expect_error(f2("txt"), "`blah` is a `character` not a `numeric`")
  expect_error(f2(2),"`f1\\(\\)` equals `2`, not `1`") #TODO: typeial case return values, e.g. f2()
})

test_that("visibility preserved", {
  type("numeric", check = function(x) is.numeric(x))
  f1 <- function(foo = ? numeric) {
    foo
  }
  f2 <- add_checks(f1)
  res <- withVisible(f2(1))
  expect_true(res$visible)
  expect_equal(res$value, 1)

  f1 <- function(foo = ? numeric) {
    invisible(foo)
  }
  f2 <- add_checks(f1)
  res <- withVisible(f2(1))
  expect_false(res$visible)
  expect_equal(res$value, 1)
})
