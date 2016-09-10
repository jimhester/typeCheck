#' Define a new type
#'
#' @param check A checking function to apply if that type is specified (optional). The
#' function is passed one argument, the value of the object.
#' @param error An error function to apply if the check fails.
#' (optional). The function is passed the object name and value in two arguments.
#' @param document A character vector that contains the documentation
#' annotation for the type, optional and currently unused.
#' @param machine_type A character vector that contains the machine type annotation
#' for the type, optional and currently unused.
#' @param ... Additional optional fields.
#' @export
#' @examples
#' type.character <- type_define( check = is.character)
#' type.integer <- type_define( check = is.integer)
#' prefix <- type_check(function(str = ? character, len = ? integer) {
#'   substring(str, 1, len)
#' })
#' \dontrun{
#'  prefix(10, 1), # `str` is a `double` not a `character`
#'  prefix("foo", NULL), # `len` is a `NULL` not a `integer`
#' }
type_define <- function(
  check = function(x) TRUE,
  error = function(obj_name, obj_value, type) {
      sprintf("`%s` is a `%s` not a `%s`.",
        obj_name,
        if (is.object(obj_value)) class(obj_value)[[1L]]
        else typeof(obj_value),
        type)
  },
  document = character(),
  machine_type = character(),
  ...){
  function(type) {
    structure(class = c(paste(type, "type", sep = "_"), "type"),
      list(
        name = type,
        check = check,
        document = document,
        error = error,
        machine_type = machine_type))
  }
}

#' Retrieve a given type if defined
#'
#' The default method signals an error
#' @param name Type name to retrieve
#' @return A type object
#' @export
type <- function(name) {
  UseMethod("type", structure(list(), class = name))
}

#' @export
type.default <- function(name) {
  stop(sQuote(name), " is an undefined type", call. = FALSE)
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
  bquote({
    `_value_` <- withVisible(.(x))
    `_type_` <- typeCheck::type(.(type))
    if (!isTRUE(`_type_`$check(`_value_`$value)))
      stop(`_type_`$error(.(name), `_value_`$value, .(type)), call. = FALSE)
    if (`_value_`$visible) `_value_`$value else invisible(`_value_`$value)
  })
}

#' Add type checks to annotated code
#'
#' This function adds type checking to code annotated with types.
#' @param x The function or expression to be modified
#' @param where The location to add type checks in a function, defaults to
#' adding them everywhere (if annotations exist).
#' @return The modified code, if the input is a function the returned object
#' has class \sQuote{checked_function} and the print method print the original
#' function definition rather than the modified code. If you would like to
#' inspect the modified code use \code{body(x)}.
#' @export
#' @examples
#' library(types)
#' type.unary <- type_define(
#'   check = function(x) length(x) == 1,
#'   error = function(n, v, t) sprintf("`%s` has length `%s`, not `1`", n, length(v)))
#' type.numeric <- type_define( check = function(x) is.numeric(x))
#' type.equals_one <- type_define(
#'   check = function(x) x == 1,
#'   error = function(n, v, t) sprintf("`%s` equals `%s`, not `1`", n, deparse(v)))
#' f <- function(blah = ? unary) { blah ? numeric } ? equals_one
#' ff <- type_check(f)
#'
#' ff(1)
#' \dontrun{
#'  ff(1:2) # `blah` has length `2`, not `1`
#'  ff("txt") # `blah` is a `character` not a `numeric`
#'  ff(2) # `f1\\(\\)` equals `2`, not `1`
#' }
type_check <- function (x, where = c("arguments", "body", "return")) {
  where <- match.arg(where, several.ok = TRUE)

  recurse <- function(y) {
    lapply(y, type_check, where = where)
  }
  if (is.atomic(x) || is.name(x)) {
    x
  }
  else if (is.call(x)) {
    if (type_exists(x) && length(x) == 3L && "body" %in% where) {
      type <- as.character(x[[3]])
      add_check(x, type, as.character(x[[2]]))
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
    has_return_type <- type_exists(body)

    # check for type on function return
    if (has_return_type && "return" %in% where) {
      label <- paste0(deparse(substitute(x)), "()")
      type <- as.character(body[[3]])
      if ("body" %in% where) {
         body[[2]] <- Recall(body[[2]], where = where)
      }
      body <- add_check(body, type, label)
    } else if ("body" %in% where) { # Otherwise just recall on the body

      # If it has a return annotation, we only want to leave it intact and only
      # Recall on the rest of the body
      if (has_return_type) {
        body[[2]] <- Recall(body[[2]], where = where)
      } else {
        body <- Recall(body, where = where)
      }
    }

    # Add argument checks if needed
    if (length(chks) > 0 && "arguments" %in% where){
      body <- as.call(c(as.symbol("{"), chks, body))
    }

    # If there were no annotations, just return x unchanged
    if (identical(body, body(x)) && identical(fmls, formals(x))) {
       return(x)
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
  else { # nocov start
    stop("Unknown language class: ", paste(class(x), collapse = "/"),
      call. = FALSE)
  } # nocov end
}

#' Add type checking to all functions in a package
#'
#' This function can be placed anywhere in the package code and will be
#' installed just before the lazy load database is populated. The idea for this
#' function and the implementation implementation borrows heavily from
#' \href{https://github.com/gaborcsardi/argufy/blob/bceef7904eef178c9aa67709940a29c1483c3c13/R/transform.R#L115-L142}{argufy::argufy_me()}.
#' @param ... Additional arguments passed to \code{\link{type_check}}
#' @export
#' @examples
#' \dontrun{
#' typeCheck::type_check_package()
#' }
type_check_package <- function(...) {
  # Trace makeLazyLoadDB
  suppressMessages(trace("makeLazyLoadDB", print = FALSE, where = asNamespace("tools"),
    function() {
      # installing documentation, so just return
      if (is.na(find_parent("code2LazyLoadDB"))) {
        return()
      }

      # Get the from argument from the makeLazyLoadDB call, which is the package
      # namespace
      env <- get("from", sys.frame(find_parent("makeLazyLoadDB"))) # nocov
      type_check_environment(env, ...)
    }))

  # Trace devtools:::run_ns_load_actions if devtools is loaded
  if ("devtools" %in% loadedNamespaces()) { # nocov start
    suppressMessages(trace("run_ns_load_actions", print = FALSE, where = asNamespace("devtools"),
        function() {
          pkg <- get("pkg", envir = sys.frame(find_parent("run_ns_load_actions")))
          env <- getExportedValue("devtools", "ns_env")(pkg)
          type_check_environment(env, ...)
        }))
  } #nocov end
}

type_check_environment <- function(env, ...) {
  # Modify all the top level functions in the package
  objects <- ls(env, all.names = TRUE)
  for (name in objects) {
    fun <- get(name, envir = env)
    if (!is.function(fun)) {
      next
    }
    fun <- type_check(fun, ...)
    assign(name, fun, envir = env)
  }
}

#' @export
print.checked_function <- function(x, ...) {
  print(attr(x, "original_fun"))
}
