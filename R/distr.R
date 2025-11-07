#' Sample from a discrete-uniform distribution
#'
#' Create random values from a range of integers with equal probability for each
#' integer value in between a lower and an upper bound. The boundaries are part
#' of the set of possible values, too.
#'
#' @param n A single integerish value representing the number of samples
#' @param min A vector of integerish values, either of length 1 or of length
#' `n`, representing the lower bound of the distribution.
#' @param max A vector of integerish values, either of length 1 or of length
#' `n`, representing the upper bound of the distribution.
#'
#' @returns A vector of type double of length `n` with integerish values between
#' `min` and `max` and equal probability for all values.
#'
#' @export
#'
#' @examples
#' rdiscunif(n = 1e2, min = 3, max = 12)
#'
#' rdiscunif(n = 3, min = c(1, 2, 3), max = c(10, 11, 12))
rdiscunif <- function(n, min, max) {
  # Check inputs
  checkmate::assert_integerish(n,
    lower = 1, any.missing = FALSE, len = 1,
    null.ok = FALSE
  )
  checkmate::assert_integerish(min,
    any.missing = FALSE, null.ok = FALSE,
    min.len = 1, max.len = n
  )
  if (length(min) > 1) {
    checkmate::assert_integerish(min, len = n)
  }

  checkmate::assert_integerish(max,
    any.missing = FALSE, null.ok = FALSE,
    min.len = 1, max.len = n
  )
  if (length(max) > 1) {
    checkmate::assert_integerish(max, len = n)
  }

  if (max %>% magrittr::is_less_than(min) %>% all()) {
    cli::cli_abort("Argument `max` must be greater then or equal to argument
                   `min`!")
  }

  # Sample and return
  round(stats::runif(n = n, min = min - 0.499999, max = max + 0.499999), 0)
}
