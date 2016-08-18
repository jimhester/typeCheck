spec <- function(
  name,
  check = NULL,
  document = NULL,
  error = NULL,
  machine_type = NULL,
  ...) {
  s <- structure(class = c(paste(name, "spec", sep = "_"), "spec"),
    list(
      name = name,
      check = check,
      document = document,
      error = error,
      machine_type = machine_type))
  spec_register(s)

  invisible(s)
}

specs <- new.env(parent = emptyenv())

spec_register <- function(x) {
  specs[[x$name]] <<- x
}
spec_clear <- function(x = NULL) {
  if (is.null(x)) {
    specs <<- new.env(parent = emptyenv())
  }
}

spec_get <- function(name) {
  if (!exists(name, specs)) {
    stop(sQuote(name), " is an undefined type", call. = FALSE)
  }
  specs[[name]]
}

# testthat:::label
label <- function(x) {
  if (is.atomic(x)) {
    format(x)
  } else if (is.name(x)) {
    as.character(x)
  } else {
    chr <- deparse(x)
    if (length(chr) > 1) {
      chr <- paste(deparse(as.call(list(x[[1]], quote(...)))), collapse = "\n")
    }
    chr
  }
}

check_create <- function(x, spec) {
  if (is.null(spec$check)) {
    return(x)
  }
  if (!is.null(spec$error)) {
    error_msg <- error
  } else {
    error_msg <- "`%s` is not of type `%s`"
  }
  bquote({
    `_value_` <- .(x)
    if (!isTRUE(.(spec$check)(`_value_`))) {
      stop(sprintf(.(error_msg), .(label(x)), .(spec$name)), call. = FALSE)
    }
    `_value_`
  })
}


add_checks <- function (x, body = NULL) {
    recurse <- function(y) {
        lapply(y, add_checks)
    }
    if (is.atomic(x) || is.name(x)) {
      x
    }
    else if (is.call(x)) {
      if (identical(x[[1]], as.symbol("?"))) {
        if (length(x) == 3) {
          spec <- spec_get(as.character(x[[3]]))

          # if body is null we are within a function body, so add the checks immediately before the previous call
          if (is.null(body)) {
            check_create(x[[2]], spec)
          }
        } else {
          # ?(x) call, This should only occur for arguments with no default
          spec <- spec_get(as.character(x[[2]]))
          quote(expr = )
        }
      } else {
        as.call(recurse(x))
      }
    }
    else if (is.function(x)) {
        formals(x) <- Recall(formals(x), body = body(x))
        body(x) <- Recall(body(x))
        x
    }
    else if (is.pairlist(x)) {
        as.pairlist(recurse(x))
    }
    else if (is.expression(x)) {
        as.expression(recurse(x))
    }
    else if (is.list(x)) {
        recurse(x)
    }
    else {
        stop("Unknown language class: ", paste(class(x), collapse = "/"),
            call. = FALSE)
    }
}
