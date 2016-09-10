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

# from https://github.com/gaborcsardi/argufy/blob/bceef7904eef178c9aa67709940a29c1483c3c13/R/utils.R#L2-L8
find_parent <- function(name) {
  name <- as.symbol(name)
  calls <- sys.calls()
  for (i in seq_along(calls)) {
    if (identical(calls[[i]][[1]], name)) return(i)
  }
  NA_integer_
}
