# From https://github.com/gaborcsardi/argufy/blob/bceef7904eef178c9aa67709940a29c1483c3c13/tests/testthat/helper.R
install_tmp_pkg <- function(pkg, expr) {
  on.exit(unloadNamespace(pkg))
  tmp <- tempfile()
  dir.create(tmp)
  withr::with_libpaths(
    tmp, action = "prefix",
    withr::with_envvar(
      c(R_LIBS = tmp, R_LIBS_USER = tmp, R_LIBS_SITE = tmp,
        R_PROFILE_USER = tempfile(), R_TESTS = NA_character_),
      {
        install.packages(pkg, repos = NULL, type = "source", quiet = TRUE)
        expr
      })
    )
}
