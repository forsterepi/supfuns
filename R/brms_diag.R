#' Create diagnostic summaries for `brms` models
#'
#' @param fit An object of class `brmsfit`.
#' @param max_treedepth A single integerish value indicating the first value
#' that is reported as "reaching the maximum treedepth". Default is 10. It is
#' included as a parameter because I could not find a reliable way to extract
#' the value from the `fit` object.
#'
#' @details
#' \describe{
#' \item{Divergencies (div)}{Number of divergencies}
#' \item{Tree depth (tree)}{Number of interations that reached the maximum tree
#' depth}
#' \item{E-BFMI (bfmi)}{Number of chains that have a value below 0.2. The
#' maximum number is the number of chains, because E-BFMI is evaluated by chain,
#' not by iteration.}
#' \item{Rhat (rhat)}{Number of parameters with a Rhat value above 1.01}
#' \item{Bulk effective sample size (ess_bulk)}{Number of parameters with a bulk
#' effective sample size below 100 multiplied by the number of chains, i.e.,
#' below 400 for if 4 chains have been used}
#' \item{Tail effective sample size (ess_tail)}{Number of parameters with a tail
#' effective sample size below 100 multiplied by the number of chains, i.e.,
#' below 400 for if 4 chains have been used}
#' }
#'
#' @returns A list with six elements named `div`, `tree`, `bfmi`, `rhat`,
#' `ess_bulk`, and `ess_tail` containing one integer each. See details for more
#' information.
#' @export
#'
#' @examples
#' d <- data.frame(y = c(0, 1, 0, 1, 0, 1, 1, 0, 1, 1))
#' f <- brms::brm(
#'   y ~ 1, data = d, family = brms::bernoulli(),
#'   refresh = 0, silent = 2
#' )
#' brms_diag(f)
brms_diag <- function(fit, max_treedepth = 10) {
  # Check inputs
  checkmate::assert_class(fit, classes = "brmsfit")
  checkmate::assert_integerish(max_treedepth,
    len = 1, any.missing = FALSE, null.ok = FALSE, lower = 1
  )

  # Create output
  list(
    div = brms_diag_div(fit),
    tree = brms_diag_tree(fit, max_treedepth = max_treedepth),
    bfmi = brms_diag_bfmi(fit),
    rhat = brms_diag_rhat(fit),
    ess_bulk = brms_diag_ess_bulk(fit),
    ess_tail = brms_diag_ess_tail(fit)
  )
}

#' Evaluate divergencies in brmsfit
#'
#' Extracts the number of divergent transitions from a `brmsfit` object.
#'
#' @param fit An object of class `brmsfit` passed from `brms_diag()`.
#'
#' @returns A single integer indicating the number of divergencies.
#'
#' @noRd
brms_diag_div <- function(fit) {
  brms::nuts_params(fit, pars = "divergent__") %>%
    magrittr::extract2("Value") %>%
    sum(na.rm = TRUE)
}

#' Evaluate treedepth in brmsfit
#'
#' Extracts the number of interations the maximum treedepth has been hit from a
#' `brmsfit` object.
#'
#' @param fit An object of class `brmsfit` passed from `brms_diag()`.
#' @param max_treedepth A single integerish value indicating the first value
#' that is reported as "reaching the maximum treedepth". Default is 10. It is
#' included as a parameter because I could not find a reliable way to extract
#' the value from the `fit` object.
#'
#' @returns A single integer indicating the number of interations that reached
#' the maximum tree depth.
#'
#' @noRd
brms_diag_tree <- function(fit, max_treedepth = 10) {
  checkmate::assert_integerish(max_treedepth,
    len = 1, any.missing = FALSE, null.ok = FALSE, lower = 1
  )

  brms::nuts_params(fit, pars = "treedepth__") %>%
    magrittr::extract2("Value") %>%
    magrittr::is_weakly_greater_than(max_treedepth) %>%
    sum(na.rm = TRUE)
}

#' Evaluate E-BFMI in brmsfit
#'
#' Extract the number of chains that violate the E-BFMI criterion from a
#' `brmsfit` object.
#'
#' @param fit An object of class `brmsfit` passed from `brms_diag()`.
#'
#' @returns A single integer indicating the number of chains that have a value
#' below 0.2. The maximum number is the number of chain, because E-BFMI is
#' evaluated by chains, not by iteration.
#'
#' @noRd
brms_diag_bfmi <- function(fit) {
  1:brms::nchains(fit) %>%
    purrr::map(
      \(x) brms::nuts_params(fit, pars = "energy__") %>%
        dplyr::filter(.data$Chain == x)
    ) %>%
    purrr::map(
      \(x) x$Value
    ) %>%
    purrr::map(
      \(x) (sum(diff(x)^2) / length(x)) / (stats::var(x))
    ) %>%
    unlist() %>%
    magrittr::is_less_than(0.2) %>%
    sum(na.rm = TRUE)
}

#' Evaluate Rhat in brmsfit
#'
#' Extract the number of parameters with a too high Rhat value from a `brmsfit`
#' object.
#'
#' @param fit An object of class `brmsfit` passed from `brms_diag()`.
#'
#' @returns A single integer indicating the number of parameters with a Rhat
#' value above 1.01.
#'
#' @noRd
brms_diag_rhat <- function(fit) {
  fit %>%
    brms::as_draws_array() %>%
    posterior::summarise_draws("rhat") %>%
    magrittr::extract2("rhat") %>%
    magrittr::is_greater_than(1.01) %>%
    sum(na.rm = TRUE)
}

#' Evaluate bulk effective sample size in brmsfit
#'
#' Extract the number of parameters with too low bulk effective sample size from
#' a `brmsfit` object.
#'
#' @param fit An object of class `brmsfit` passed from `brms_diag()`.
#'
#' @returns A single integer indicating the number of parameters with a bulk
#' effective sample size below 100 * the number of chains, i.e., below 400 for
#' if 4 chains have been used. This threshold has been described in the CmdStan
#' User Guide on the Stan homepage (not cmdstanr).
#'
#' @noRd
brms_diag_ess_bulk <- function(fit) {
  fit %>%
    brms::as_draws_array() %>%
    posterior::summarise_draws("ess_bulk") %>%
    magrittr::extract2("ess_bulk") %>%
    magrittr::is_weakly_less_than(100 * brms::nchains(fit)) %>%
    sum(na.rm = TRUE)
}

#' Evaluate tail effective sample size in brmsfit
#'
#' Extract the number of parameters with too low tail effective sample size from
#' a `brmsfit` object.
#'
#' @param fit An object of class `brmsfit` passed from `brms_diag()`.
#'
#' @returns A single integer indicating the number of parameters with a tail
#' effective sample size below 100 * the number of chains, i.e., below 400 for
#' if 4 chains have been used. This threshold has been described in the CmdStan
#' User Guide on the Stan homepage (not cmdstanr).
#'
#' @noRd
brms_diag_ess_tail <- function(fit) {
  fit %>%
    brms::as_draws_array() %>%
    posterior::summarise_draws("ess_tail") %>%
    magrittr::extract2("ess_tail") %>%
    magrittr::is_weakly_less_than(100 * brms::nchains(fit)) %>%
    sum(na.rm = TRUE)
}
