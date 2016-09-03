spec <- function(
  name,
  check = NULL,
  document = NULL,
  error = function(obj_name, obj_value) {
      sprintf("`%s` is a `%s` not a `%s`.",
        obj_name,
        if (is.object(obj_value)) class(obj_value)[[1L]]
        else typeof(obj_value),
        name)
  },
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
    #if (length(chr) > 1) {
      #chr <- paste(deparse(as.call(list(x[[1]], quote(...)))), collapse = "\n")
    #}
    paste(chr, collapse = "\n")
  }
}

object_type <- function(x) {
  if (is.object(x)) {
    return(class(x)[[1L]])
  }
  return(typeof(x))
}

spec_exists <- function(x) {
  is.call(x) && x[[1]] %===% as.symbol("?")
}

spec_check <- function(x, type, name = label(x)) {
  if (is.null(spec_get(type))) {
    stop("`", type, "` is an undefined type", call. = FALSE)
  }
  bquote({
    `_value_` <- .(x)
    `_spec_` <- typeCheck::spec_get(.(type))
    if (!isTRUE(`_spec_`$check(`_value_`))) {
      stop(`_spec_`$error(.(name), `_value_`), call. = FALSE)
    }
    `_value_`
  })
}

braced_body <- function(x) {
  is.call(x) && x[[1]] %===% as.symbol("{")
}

add_checks <- function (x) {
  recurse <- function(y) {
    lapply(y, add_checks)
  }
  if (is.atomic(x) || is.name(x)) {
    x
  }
  else if (is.call(x)) {
    if (spec_exists(x) && length(x) == 3L) {
      type <- as.character(x[[3]])
      if (is.call(x[[2]])) {
        spec_check(as.call(recurse(x)), type, as.character(x[[2]]))
      } else {
        spec_check(x, type, as.character(x[[2]]))
      }
    } else {
      as.call(recurse(x))
    }
  }
  else if (is.function(x)) {
    fmls <- formals(x)
    chks <- list()
    for (i in seq_along(fmls)) {
      if (spec_exists(fmls[[i]])) {
        if (length(fmls[[i]]) == 2) { # no default argument
          type <- as.character(fmls[[i]][[2]])
          #fmls[[i]] <- quote(expr = )
        } else if (length(fmls[[i]]) == 3) { # default argument
          type <- as.character(fmls[[i]][[3]])
          #fmls[[i]] <- fmls[[i]][[2]]
        }
        chks[[i]] <- spec_check(as.symbol(names(fmls)[[i]]), type)
      }
    }

    body <- body(x)
    # check for type on function return
    if (spec_exists(body)) {
      label <- paste0(deparse(substitute(x)), "()")
      type <- as.character(body[[3]])
      body[[2]] <- as.call(c(as.symbol("{"), chks, Recall(body[[2]])))
      body <- spec_check(body, type, label)
    } else {
      body <- as.call(c(as.symbol("{"), chks, Recall(body)))
    }

    formals(x) <- fmls
    body(x) <- body
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
