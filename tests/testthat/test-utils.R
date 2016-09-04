context("utils")

test_that("quoting functions work as expected", {
  expect_identical(sQuote(NULL), character())
  expect_identical(sQuote(character()), character())

  expect_identical(sQuote("x"), "'x'")
  expect_identical(bQuote("x"), "`x`")
  expect_identical(dQuote("x"), "\"x\"")
})
