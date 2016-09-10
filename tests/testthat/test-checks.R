suppressPackageStartupMessages(library(types))
context("type_check")

test_that("functions without checks", {
  f <- function(x, y = 10) x + 10
  f2 <- type_check(f)
  expect_identical(body(f2), body(f))
  expect_identical(formals(f2), formals(f))

  f <- function() { }
  f2 <- type_check(f)
  expect_equal(body(f2), body(f))
  expect_identical(formals(f2), formals(f))

  f <- function(x = 10 + 10, y = 20 && FALSE) { x * y }
  f2 <- type_check(f)
  expect_equal(body(f2), body(f))
  expect_identical(formals(f2), formals(f))
})

test_that("type_check fails if an undefined type", {
  f1 <- function(blah = ? numeric) blah + 1
  expect_error((type_check(f1))(1), "'numeric' is an undefined type")

  f2 <- function(blah = 1 ? numeric) blah + 1
  expect_error((type_check(f2))(), "'numeric' is an undefined type")
})

test_that("type_check adds a check if a defined type in formals", {
  type.numeric <- type_define("numeric", check = function(x) is.numeric(x))
  f1 <- function(blah = 1 ? numeric) blah
  f2 <- type_check(f1)
  expect_error(f2(), NA)
  expect_error(f2(1), NA)
  expect_error(f2("character"), "`blah` is a `character` not a `numeric`.")

  f1 <- function(blah = ? numeric) blah
  f2 <- type_check(f1)
  expect_error(f2(1), NA)
  expect_error(f2("character"), "`blah` is a `character` not a `numeric`.")

  f1 <- function(blah = ? numeric) { blah <- blah + 1 ; blah}
  f2 <- type_check(f1)
  expect_error(f2(1), NA)
  expect_equal(f2(1), 2)
  expect_error(f2("character"), "`blah` is a `character` not a `numeric`.")

  f1 <- function(blah = ? numeric, foo) { blah }
  f2 <- type_check(f1)
  expect_error(f2(1), NA)
  expect_error(f2("character"), "`blah` is a `character` not a `numeric`.")
})

test_that("type_check works with a call", {
  type.numeric <- type_define("numeric", check = function(x) is.numeric(x))
  type.unary <- type_define("unary", check = function(x) length(x) == 1,
    error = function(n, v, t) sprintf("`%s` has length `%s`, not `1`", n, length(v)))

  f1 <- function(x) x ? numeric ? unary
  f2 <- type_check(f1)

  expect_error(f2(1), NA)
  expect_error(f2("1"), "`x` is a `character` not a `numeric`.")
  expect_error(f2(c(1, 2)), "`f1\\(\\)` has length `2`, not `1`")
})

test_that("type_check adds a check if a defined type in body", {
  type.numeric <- type_define("numeric", check = function(x) is.numeric(x))
  f1 <- function(blah) blah ? numeric
  f2 <- type_check(f1)
  expect_error(f2(1), NA)
  expect_error(f2("character"), "`f1\\(\\)` is a `character` not a `numeric`.")
})

test_that("compound checks", {
  type.unary <- type_define("unary",
    check = function(x) length(x) == 1,
    error = function(n, v, t) sprintf("`%s` has length `%s`, not `1`", n, length(v)))
  type.numeric <- type_define("numeric", check = function(x) is.numeric(x))
  type.equals_one <- type_define("equals_one",
    check = function(x) x == 1,
    error = function(n, v, t) sprintf("`%s` equals `%s`, not `1`", n, deparse(v)))
  f1 <- function(blah = ? unary) { blah ? numeric } ? equals_one
  f2 <- type_check(f1)

  expect_error(f2(1:2), "`blah` has length `2`, not `1`")
  expect_error(f2("txt"), "`blah` is a `character` not a `numeric`")
  expect_error(f2(2),"`f1\\(\\)` equals `2`, not `1`")
})

test_that("restricted checks should only be included where requested", {
  type.unary <- type_define("unary",
    check = function(x) length(x) == 1,
    error = function(n, v, t) sprintf("`%s` has length `%s`, not `1`", n, length(v)))
  type.numeric <- type_define("numeric", check = function(x) is.numeric(x))
  type.equals_one <- type_define("equals_one",
    check = function(x) x == 1,
    error = function(n, v, t) sprintf("`%s` equals `%s`, not `1`", n, deparse(v)))
  f1 <- function(blah = ? unary) { blah ? numeric } ? equals_one

  expect_error(type_check(f1, "arguments")(1:2), "`blah` has length `2`, not `1`")
  expect_error(type_check(f1, "arguments")("txt"), NA)
  expect_error(type_check(f1, "arguments")(2), NA)

  expect_error(type_check(f1, "return")(1:2), "`f1\\(\\)` equals `1:2`, not `1`")
  expect_error(type_check(f1, "return")("txt"), "`f1\\(\\)` equals `\"txt\"`, not `1`")
  expect_error(type_check(f1, "return")(2), "`f1\\(\\)` equals `2`, not `1`")

  expect_error(type_check(f1, "body")(1:2), NA)
  expect_error(type_check(f1, "body")("txt"), "`blah` is a `character` not a `numeric`")
  expect_error(type_check(f1, "body")(2), NA)
})

test_that("checks are found recursively", {
  type.numeric <- type_define("numeric", check = function(x) is.numeric(x))

  f <- function(x) {
    ff <- function(y) y ? numeric
    ff(x)
  }
  f2 <- type_check(f)

  expect_error(f2("txt"), "`y` is a `character` not a `numeric`")
})

test_that("visibility preserved", {
  type.numeric <- type_define("numeric", check = function(x) is.numeric(x))
  f1 <- function(foo = ? numeric) {
    foo
  }
  f2 <- type_check(f1)
  res <- withVisible(f2(1))
  expect_true(res$visible)
  expect_equal(res$value, 1)

  f1 <- function(foo = ? numeric) {
    invisible(foo)
  }
  f2 <- type_check(f1)
  res <- withVisible(f2(1))
  expect_false(res$visible)
  expect_equal(res$value, 1)
})

test_that("print.type_check prints the original function definition", {
  type.numeric <- type_define("numeric", check = function(x) is.numeric(x))
  f1 <- function(foo = ? numeric) {
    foo
  }
  f2 <- type_check(f1)
  expect_equal(
    capture_output(print(f2)),
    capture_output(print(f1)))
})

clear_pkgconfig <- function() {
  remove(list = ls(envir = pkgconfig:::config), envir = pkgconfig:::config)
}
clear_pkgconfig()
