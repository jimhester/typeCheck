#' @importFrom typeCheck type type_define
NULL

type.character <- type_define("character", check = is.character)
type.integer <- type_define("integer", check = is.integer)

prefix <- function(str = ? character, len = ? integer) {
  substring(str, 1, len) ? character
}

suffix <- function(str = ? character, len = ? integer) {
  substring(str, nchar(str) - len + 1, nchar(str))
} ? character

typeCheck::type_check_package()
