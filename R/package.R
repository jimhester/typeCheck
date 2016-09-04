types <- new.env(parent = emptyenv())

#' @export
type <- function(
  name,
  check = NULL,
  error = function(obj_name, obj_value) {
      sprintf("`%s` is a `%s` not a `%s`.",
        obj_name,
        if (is.object(obj_value)) class(obj_value)[[1L]]
        else typeof(obj_value),
        name)
  },
  document = NULL,
  machine_type = NULL,
  ...) {
  res <- structure(class = c(paste(name, "type", sep = "_"), "type"),
    list(
      name = name,
      check = check,
      document = document,
      error = error,
      machine_type = machine_type))
  type_register(res)

  invisible(res)
}

type_register <- function(x) {
  types[[x$name]] <- x
}

#' @export
type_get <- function(name) {
  if (!exists(name, types)) {
    stop(sQuote(name), " is an undefined type", call. = FALSE)
  }
  types[[name]]
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
    paste(chr, collapse = "\n")
  }
}

type_exists <- function(x) {
  is.call(x) && x[[1]] %===% as.symbol("?")
}

add_check <- function(x, type, name = label(x)) {
  if (is.null(type_get(type))) {
    stop("`", type, "` is an undefined type", call. = FALSE)
  }
  bquote({
    `_value_` <- withVisible(.(x))
    `_type_` <- typeCheck::type_get(.(type))
    if (!isTRUE(`_type_`$check(`_value_`$value)))
      stop(`_type_`$error(.(name), `_value_`$value), call. = FALSE)
    if (`_value_`$visible) `_value_`$value else invisible(`_value_`$value)
  })
}

#' @export
type_check <- function (x) {
  recurse <- function(y) {
    lapply(y, type_check)
  }
  if (is.atomic(x) || is.name(x)) {
    x
  }
  else if (is.call(x)) {
    if (type_exists(x) && length(x) == 3L) {
      type <- as.character(x[[3]])
      if (is.call(x[[2]])) {
        add_check(as.call(recurse(x)), type, as.character(x[[2]]))
      } else {
        add_check(x, type, as.character(x[[2]]))
      }
    } else {
      as.call(recurse(x))
    }
  }
  else if (is.function(x)) {
    fmls <- formals(x)
    chks <- list()
    for (i in seq_along(fmls)) {
      if (type_exists(fmls[[i]])) {
        if (length(fmls[[i]]) == 2) { # no default argument
          type <- as.character(fmls[[i]][[2]])
        } else if (length(fmls[[i]]) == 3) { # default argument
          type <- as.character(fmls[[i]][[3]])
        }
        chks[[length(chks) + 1]] <- add_check(as.symbol(names(fmls)[[i]]), type)
      }
    }

    body <- body(x)

    # check for type on function return
    if (type_exists(body(x))) {
      label <- paste0(deparse(substitute(x)), "()")
      type <- as.character(body[[3]])
      body[[2]] <- as.call(c(as.symbol("{"), chks, Recall(body[[2]])))
      body <- add_check(body, type, label)
    } else { # Otherwise just recall on the body
      body <- Recall(body(x))
    }

    # Add argument checks if needed
    if (length(chks) > 0){
      body <- as.call(c(as.symbol("{"), chks, body))
    }

    res <- x
    formals(res) <- fmls
    body(res) <- body
    class(res) <- c("checked_function", class(x))
    attr(res, "original_fun") <- x
    res
  } else if (is.pairlist(x)) {
    as.pairlist(recurse(x))
  }
  else {
    stop("Unknown language class: ", paste(class(x), collapse = "/"),
      call. = FALSE)
  }
}

#' @export
print.checked_function <- function(x, ...) {
  print(attr(x, "original_fun"))
}
