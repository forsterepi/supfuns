# Avoid the devtools::check() NOTE that the . used in piping is a non-declared
# variable
utils::globalVariables(".")

#' @importFrom rlang .data :=
NULL
