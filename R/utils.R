sQuote <- function(x, quote = "'") {
  if (!length(x)) {
    return(character())
  }
  paste0(quote, x, quote)
}

bQuote <- function(x) sQuote(x, "`")
dQuote <- function(x) sQuote(x, "\"")

`%===%` <- function(x, y) identical(x, y)

`%!==%` <- Negate(`%===%`)
