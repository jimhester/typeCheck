sQuote <- function(x) {
  if (!length(x)) {
    return(character())
  }
  paste0("'", x, "'")
}

`%===%` <- identical

`%!==%` <- Negate(identical)
