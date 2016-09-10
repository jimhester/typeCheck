context("type_check_package")

test_that("type_check_package works as an .onLoad replacement", {
  install_tmp_pkg("pkg1", {
    expect_error(pkg1::prefix(10, 1), "`str` is a `double` not a `character`")
    expect_error(pkg1::prefix("foo", NULL), "`len` is a `NULL` not a `integer`")

    expect_equal(pkg1::prefix("foo", 1L), "f")

    expect_error(pkg1::suffix(10, 1), "`str` is a `double` not a `character`")
    expect_error(pkg1::suffix("foo", NULL), "`len` is a `NULL` not a `integer`")
    expect_equal(pkg1::suffix("foo", 1L), "o")
  })
})
