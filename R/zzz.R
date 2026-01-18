# Avoid the devtools::check() NOTE that the . used in piping is a non-declared
# variable
utils::globalVariables(".")

#' @importFrom rlang .data :=
NULL

# devtools::check() does not realize that package poLCA is needed in Imports
# Strategy suggested in R packages (2e), 11.4.1.1
ignore_unused_imports <- function() {
  poLCA::poLCA
}
