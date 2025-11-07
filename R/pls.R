#' Sample seeds
#'
#' A simple helper function for drawing random integers for use in `set.seed()`
#' or `seed` arguments of functions. Let's not always use `123`!
#'
#' @param n An integer or double representation of an integer, describing the
#' number of seeds to sample. Default is 1.
#' @param print TRUE or FALSE, describing if the seeds should be printed in a
#' copy-friendly format in the console or not. Default is TRUE, i.e., copy-
#' friendly. In order to create a vector of seeds to be used, set `print` to
#' FALSE.
#'
#' @returns Either prints seeds in copy-friendly format in the console (`print`
#' = TRUE), or returns a vector of integers.
#'
#' @export
#'
#' @examples
#' seed.pls(5)
seed.pls <- function(n = 1, print = TRUE) {
  # Check input
  rlang::check_installed("checkmate", "stringi")

  checkmate::assert_flag(print, na.ok = FALSE, null.ok = FALSE)
  checkmate::assert_integerish(n,
    lower = 1L,
    len = 1L,
    any.missing = FALSE,
    all.missing = FALSE,
    null.ok = TRUE
  )

  # Sample
  samples <- sample.int(1e6, n)

  # Make sure all sampled seeds are unique
  while (samples %>%
    unique() %>%
    length() %>%
    magrittr::equals(n) %>%
    magrittr::not()) {
    samples <- sample.int(1e6, n)
  }

  # Return
  if (print) {
    samples %>%
      stringi::stri_c(collapse = "\n") %>%
      cat()
  } else {
    samples
  }
}
