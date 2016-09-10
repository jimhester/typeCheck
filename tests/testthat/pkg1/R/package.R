#' @importFrom typeCheck type type_define
NULL

type.character <- type_define(check = is.character)
type.integer <- type_define(check = is.integer)

#' @export
prefix <- function(str = ? character, len = ? integer) {
  substring(str, 1, len) ? length_one
}

#' @export
suffix <- function(str = ? character, len = ? integer) {
  substring(str, nchar(str) - len + 1, nchar(str))
} ? character

.onLoad <- function(libname, pkgname) {
  typeCheck::type_check_package(asNamespace(pkgname))
}
