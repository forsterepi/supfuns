#' Extract symbols from AST
#'
#' Extract all symbols from abstract syntax trees.
#'
#' @param x An expression (formed, e.g., by wrapping inside of `rlang::expr()`)
#'
#' @returns A character vector containing all symbol names. If no symobl are in
#' the call, returns an empty character vector, i.e., character(0).
#'
#' @export
#'
#' @examples
#' extract.symbols.from.ast(rlang::expr(is.na(y)))
extract.symbols.from.ast <- function(x) {
  if (rlang::is_syntactic_literal(x)) {
    # If x is a constant, return an empty character vector
    character(0)
  } else if (is.symbol(x)) {
    # If x is a symbol, return the symbol as string
    rlang::as_string(x)
  } else if (is.call(x)) {
    # If x is a call, apply the function to all elements of the call apart from
    # the first one, which contains the function name. This also avoids that
    # package names and function are included when `::` is used, because the
    # call to `::` is also in the first position instead of the function name.
    # unlist() puts the individual elements together. as.character() avoids
    # problems when some elements have names, e.g., function arguments with
    # names.
    lapply(x, extract.symbols.from.ast) %>%
      unlist() %>%
      as.character()
  } else if (is.pairlist(x)) {
    # Same as call
    lapply(x, extract.symbols.from.ast) %>%
      unlist() %>%
      as.character()
  } else {
    # Throw internal error for other types
    cli::cli_abort(c("x" = "Error in extract.symbols.from.ast()"))
  }
}

#' Get all function and package names
#'
#' @param include.package.names TRUE (default) or FALSE, indicating if names of
#' packages are supposed to be included as well.
#'
#' @returns A (usually large) character vector containing the names of all
#' available functions and (potentially) packages.
#'
#' @export
#'
#' @examples
#' get.all.functions()
get.all.functions <- function(include.package.names = TRUE) {
  x <- loadedNamespaces()
  funs <- sapply(x, getNamespaceExports) %>%
    unlist() %>%
    magrittr::set_attr("names", NULL)

  if (include.package.names) {
    funs <- c(funs, x)
  }

  funs
}
