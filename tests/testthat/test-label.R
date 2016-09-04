context("label")

test_that("labelling compound {} expression gives single string", {
  out <- label(quote({1 + 2}))

  expect_length(out, 1)
  expect_type(out, "character")
})

test_that("labelling atomic value formats it", {
  out <- label(100e10)

  expect_length(out, 1)
  expect_type(out, "character")
  expect_equal(out, format(100e10))
})
