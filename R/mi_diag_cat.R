#' Diagnose imputation of categorical variables
#'
#' Compare the proportions of categories in non-imputed and imputed datasets in
#' a plot
#'
#' @param imp A `mids` object, i.e., the output of function `mice::mice()`
#' @param var A single string containing the name of the variable to be plotted.
#' `var` must be factor.
#'
#' @returns A ggplot object
#' @export
#'
#' @examples
#' df <- data.frame(
#' x = rnorm(100, 0, 1),
#' y = sample(c(1, 2, 3), size = 100, replace = TRUE, prob = c(0.2, 0.3, 0.5)),
#' r = runif(100, 0, 1)
#' ) %>%
#'   dplyr::mutate(
#'     "y" := dplyr::case_when(
#'       .data$r < 0.05 ~ NA,
#'       .default = .data$y
#'     ),
#'     "r" := NULL,
#'     "y" := factor(.data$y)
#'   )
#' imp <- mice::mice(df, m = 2, maxit = 2)
#' mi_diag_cat(imp, "y")
mi_diag_cat <- function(imp, var) {
  # Check inputs
  checkmate::assert_class(imp, classes = "mids", null.ok = FALSE)
  checkmate::assert_string(var, na.ok = FALSE, min.chars = 1, null.ok = FALSE)

  imp0 <- imp %>% mice::complete(0)

  checkmate::assert_subset(var, choices = colnames(imp0), empty.ok = FALSE)
  if (!is.factor(imp0[[var]])) {
    cli::cli_abort("The variable specified in argument `var` must be a factor!")
  }

  # Create plot data
  ## Get levels
  var_levels <- imp0 %>%
    magrittr::extract2(var) %>%
    levels()

  ## Create empty plot data
  df_plot <- expand.grid(c(0:imp$m) %>%
    as.character(), var_levels) %>%
    as.data.frame() %>%
    magrittr::set_colnames(c("m", "level"))
  df_plot$val <- NA

  ## Fill plot data
  for (i in seq_len(nrow(df_plot))) {
    ### Get imputed data.frame
    df_temp <- imp %>%
      mice::complete(df_plot$m[i] %>%
        as.character() %>%
        as.integer())

    ### Get frequency of category
    df_plot$val[i] <- df_temp[, var] %>%
      magrittr::equals(df_plot$level[i] %>%
        as.character()) %>%
      sum(na.rm = T) %>%
      magrittr::divide_by(df_temp[, var] %>%
        is.na() %>%
        magrittr::not() %>%
        sum())
  }
  rm(i)

  # Plot
  df_plot %>%
    ggplot2::ggplot(ggplot2::aes(
      fill = .data$level, y = .data$val, x = .data$m
    )) +
    ggplot2::geom_bar(position = "fill", stat = "identity") +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = var, fill = "Level", y = "Proportion",
      x = "# of imputed dataset (0 = non-imputed)"
    )
}
