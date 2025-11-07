#' Run simluations based on simcausal modules
#'
#' @param dags A list of `simcausal::DAG` objects. The DAGs are not locked yet,
#' because locking is done as part of `run_sim()`. The DAGs must not include the
#' same variables multiple times, e.g., if a variable from module1 is needed in
#' module2, module 2 must not contain the variable, because when combining both
#' modules in this list, the variable would be available twice leading to an
#' error. Similarly, the order, in which DAGs are listed is important, e.g.,
#' module1 must be listed first because its variable that is used in module2
#' would otherwise not be available.
#' @param args A named list of all parameters used in `dags`, which are not
#' other variables in the DAGs. If, e.g., a variable is described be a normal
#' distribution with a fixed mean and this mean is called `mean_eg` in the code
#' that defines the DAG, then `mean_eg` must be provided in this list.
#' @param n An integerish number indicating the number of samples. Default is
#' 1000.
#' @param seed An integerish number passed to argument `rndseed` in function
#' `simcausal::sim()`.
#'
#' @returns A data.frame with columns `ID` as well as one column for each
#' variable included in the DAG. The data.frame has `n` rows. It is in wide
#' format, because variables that exist at different time points are different
#' columns in the data.frame, e.g., `asthma_6` and `asthma_7` for asthma at age
#' 6 and 7, respectively.
#'
#' @export
#'
#' @examples
#' simtest <- simcausal::DAG.empty() %>%
#'   simcausal::add.nodes(
#'     simcausal::node("x",
#'       distr = "rnorm",
#'       mean = 0,
#'       sd = 14
#'     )
#'   )
#' run_sim(dags = list(simtest), n = 5)
run_sim <- function(dags, args = NULL, n = 1e3, seed = NULL) {
  # Check inputs
  checkmate::assert_list(dags,
    types = "DAG", min.len = 1, any.missing = FALSE,
    unique = TRUE, null.ok = FALSE
  )
  checkmate::assert_list(args,
    types = "numeric", null.ok = TRUE,
    any.missing = FALSE, names = "unique"
  )
  checkmate::assert_integerish(n,
    lower = 1, any.missing = FALSE,
    len = 1, null.ok = FALSE
  )
  checkmate::assert_integerish(seed,
    lower = 1, any.missing = FALSE,
    len = 1, null.ok = TRUE
  )

  if (is.null(seed)) {
    seed <- supfuns::seed.pls(1, print = FALSE)
  }

  # Combine all modules (note: order is important and modules cannot include
  # other modules or nodes with the same name)
  combi_dag <- purrr::reduce(dags,
    simcausal::add.nodes,
    .init = simcausal::DAG.empty()
  )

  # Check if all args are available
  ca <- check_args(dag = combi_dag, args = args)
  if (length(ca) > 0) {
    cli::cli_abort(c("Not all necessary parameters have been provided in
                     `args`!",
      i = "The following parameters are missing: {ca}"
    ))
  }

  # Add args to the global environment from which simcausal takes its values
  old_args <- purrr::imap(
    args,
    \(x, idx) rlang::env_poke(env = rlang::global_env(), nm = idx, value = x)
  )

  # Try to lock the DAG
  rlang::try_fetch(
    {
      suppressWarnings(
        cap <- utils::capture.output(final_dag <- combi_dag %>%
          simcausal::set.DAG(verbose = FALSE, vecfun = c("qlogis", "plogis")))
      )
    },
    error = function(cnd) {
      cli::cli_abort("errores!")
    }
  )

  # Simulate from the locked DAG
  suppressWarnings(
    cap <- utils::capture.output(out <- final_dag %>%
      simcausal::sim(n = n, rndseed = seed, verbose = FALSE))
  )
  # Put args back to their original values
  purrr::iwalk(
    old_args,
    \(x, idx) rlang::env_poke(env = rlang::global_env(), nm = idx, value = x)
  )

  # Return
  out
}

#' Check arguments passed to `run_sim()`
#'
#' Checks if values have provided to all parameters in the specified DAGs
#'
#' @param dag The combined DAGs provided to arguments `dags` in `run_sim()`.
#' @param args The list of arguments passed to argument `args` in `run_sim()`.
#'
#' @returns A character vector with the names of the arguments/parameters, which
#' are not included in argument `args` even though they are needed for
#' simulation. If all necessary arguments/parameters have been provided, an
#' empty character vector (`character(0)`) is returned.
#'
#' @noRd
check_args <- function(dag, args) {
  # Get list of functions and packages
  fun.list <- sapply(loadedNamespaces(), getNamespaceExports) %>%
    unlist() %>%
    magrittr::set_attr("names", NULL) %>%
    c(., loadedNamespaces())

  # Get dag nodes
  dag_nodes_part1 <- names(dag) %>%
    magrittr::extract(names(dag) %>%
      stringi::stri_detect_regex("_") %>%
      magrittr::not())

  dag_nodes_part2 <- names(dag) %>%
    magrittr::extract(names(dag) %>%
      stringi::stri_detect_regex("_")) %>%
    stringi::stri_extract_first_regex("^.+(?=_)") %>%
    unique()

  dag_nodes <- c(dag_nodes_part1, dag_nodes_part2)

  # Compare to model
  params <- vector(mode = "list", length = length(dag))

  for (i in seq_along(dag)) {
    params[[i]] <- dag[[i]]$dist_params
  }

  params %<>% unlist(recursive = FALSE) %>%
    purrr::map(stringi::stri_c, collapse = "") %>%
    purrr::map(rlang::parse_expr) %>%
    purrr::map(supfuns::extract.symbols.from.ast) %>%
    unlist() %>%
    unique() %>%
    magrittr::extract(!(. %in% c(dag_nodes, fun.list)))
  rm(fun.list)

  out <- params %>% magrittr::extract(params %>%
    magrittr::is_in(names(args)) %>%
    magrittr::not())

  out
}
