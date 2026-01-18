#' Print an object of class `poLCA`
#'
#' @param lca An object of class `poLCA` created by `poLCA::poLCA()`
#' @param cut TRUE or FALSE indicating if values below 0 or above 1 in the
#' confidence interval (CI) should be replaced by 0 and 1
#' @param nice TRUE or FALSE indicating if estimate and CI should be formatted
#' nicely
#'
#' @returns Print a nice summary of the object passed to `lca`
#' @export
#'
#' @examples
#' f <- cbind(var1, var2, var3, var4) ~ 1
#' lca <- poLCA::poLCA(f, lca_example_data, nclass = 3, verbose = FALSE)
#' lca_print(lca)
lca_print <- function(lca, cut = FALSE, nice = TRUE) {
  # Check input
  checkmate::assert_class(lca, classes = "poLCA", null.ok = FALSE)
  checkmate::assert_flag(cut, na.ok = FALSE, null.ok = FALSE)
  checkmate::assert_flag(nice, na.ok = FALSE, null.ok = FALSE)

  lca$y %>%
    purrr::imap(\(x, idx) checkmate::assert_factor(x, .var.name = idx))

  # Prepare
  indic_levels <- lca$y %>%
    purrr::map(levels)

  total_number_levels <- indic_levels %>%
    purrr::map_int(length) %>%
    sum()

  # Create out
  out_col <- lca$call$nclass %>% magrittr::add(1)
  out_row <- 3 + ncol(lca$y) + total_number_levels

  out <- matrix(rep("", out_row * out_col), nrow = out_row, ncol = out_col) %>%
    as.data.frame() %>%
    magrittr::set_colnames(c("Var", paste0("Class", c(1:lca$call$nclass))))


  # Fill
  out[1, "Var"] <- "Latent Class Prevalences"
  out[3, "Var"] <- "Item Response Probabilities"

  for (i in 1:lca$call$nclass) {
    out[2, 1 + i] <- lca_ci_p(lca, class = i, nice = nice, cut = cut)
  }
  rm(i)

  for (i in seq_along(indic_levels)) {
    previous_rows <- 0

    if (i > 1) {
      for (j in 1:(i - 1)) {
        previous_rows %<>% magrittr::add(1 + length(indic_levels[[j]]))
      }
      rm(j)
    }

    start_row <- 4 + previous_rows

    out[start_row, "Var"] <- names(indic_levels)[i]

    for (j in 1:length(indic_levels[[i]])) {
      out[start_row + j, "Var"] <- indic_levels[[i]][j]

      for (k in 1:lca$call$nclass) {
        out[start_row + j, 1 + k] <- lca_ci(lca,
          class = k,
          var = names(indic_levels)[i],
          cat = as.integer(j),
          cut = cut,
          nice = nice
        )
      }
      rm(k)
    }
    rm(j)
  }
  rm(i)


  # Return
  return(out)
}

#' Table creation latent class model
#'
#' @param lca_list A list of objects of class `poLCA` created by
#' `poLCA::poLCA()`
#'
#' @returns A table containing information on the models
#' @export
#'
#' @examples
#' f <- cbind(var1, var2, var3, var4) ~ 1
#' lca2 <- poLCA::poLCA(f, lca_example_data, nclass = 2, nrep = 100, verbose = FALSE)
#' lca3 <- poLCA::poLCA(f, lca_example_data, nclass = 3, nrep = 100, verbose = FALSE)
#' lca_table(list(lca2, lca3))
lca_table <- function(lca_list) {
  # Check input
  checkmate::assert_list(lca_list,
    types = "poLCA", min.len = 2,
    any.missing = FALSE, null.ok = FALSE
  )

  table_out <- matrix(
    data = rep(NA, length(lca_list) * 10),
    nrow = length(lca_list), ncol = 10
  ) %>%
    as.data.frame() %>%
    magrittr::set_colnames(c(
      "nclass", "nparam", "N", "Nobs", "Gsq",
      "llik", "df", "AIC", "BIC", "ident"
    ))

  for (i in 1:length(lca_list)) {
    if (table_out %>%
      colnames() %>%
      magrittr::equals(lca_list[[i]] %>%
        lca_extract() %>%
        names()) %>%
      sum() %>%
      magrittr::equals(table_out %>% ncol())) {
      table_out[i, ] <- lca_list[[i]] %>% lca_extract()
    }
  }

  # Return
  return(table_out)
}

#' Extracts the information necessary for plotting the lca
#'
#' @param lca An object of class `poLCA` created by `poLCA::poLCA()`
#'
#' @returns A table that can be used for creating a plot that describes the LCA
#' @export
#'
#' @examples
#' f <- cbind(var1, var2, var3, var4) ~ 1
#' lca <- poLCA::poLCA(f, lca_example_data, nclass = 3, verbose = FALSE)
#' lca_plot_input(lca)
lca_plot_input <- function(lca) {
  # Check inputs
  checkmate::assert_class(lca, classes = "poLCA", null.ok = FALSE)

  # Prepare
  indic_levels <- lca$y %>%
    purrr::map(levels)

  total_number_levels <- indic_levels %>%
    purrr::map_int(length) %>%
    sum()

  # Create out
  n_class <- lca$call$nclass

  out_col <- 6
  out_row <- total_number_levels * n_class

  out <- matrix(rep("", out_row * out_col), nrow = out_row, ncol = out_col) %>%
    as.data.frame() %>%
    magrittr::set_colnames(c("class", "var", "cat", "est", "lwr", "upr"))


  # Fill output
  for (i in 1:n_class) { # loop over class

    for (j in seq_along(indic_levels)) { # loop over indicator variables

      for (k in seq_along(indic_levels[[j]])) { # loop over categories over indicator variables

        row <- ((i - 1) * total_number_levels) + (indic_levels[0:(j - 1)] %>%
          purrr::map(length) %>%
          unlist() %>%
          sum()) + k

        var_temp <- names(indic_levels)[j]
        cat_temp <- indic_levels[[j]][k]

        out$class[row] <- as.character(i)
        out$var[row] <- var_temp
        out$cat[row] <- cat_temp

        prob_ci <- lca_ci(lca,
          class = i,
          var = var_temp,
          cat = cat_temp,
          cut = T,
          nice = F
        )

        out$est[row] <- prob_ci[1] %>% round(3)
        out$lwr[row] <- prob_ci[2] %>% round(3)
        out$upr[row] <- prob_ci[3] %>% round(3)
      }
    }
  }

  # Return
  return(out)
}

#' Check LCA model identification
#'
#' @param lca An object of class `poLCA` created by `poLCA::poLCA()`
#' @param for_table TRUE or FALSE indicating if the result should be used for
#' `supfuns::lca_extract()`
#'
#' @returns A single value indicating the percentage of starting values
#' resulting in the best model
#'
#' @export
#'
#' @examples
#' f <- cbind(var1, var2, var3, var4) ~ 1
#' lca <- poLCA::poLCA(f, lca_example_data, nclass = 3, verbose = FALSE)
#' lca_ident(lca)
lca_ident <- function(lca, for_table = F) {
  # Check inputs
  checkmate::assert_class(lca, classes = "poLCA", null.ok = FALSE)
  checkmate::assert_flag(for_table, na.ok = FALSE, null.ok = FALSE)

  # Extract
  llik <- lca$attempts %>% round(1)
  count <- llik %>% plyr::count()

  perc <- count$freq[count$x == max(count$x)] %>%
    magrittr::divide_by(sum(count$freq)) %>%
    magrittr::multiply_by(100) %>%
    round(1) %>%
    format(nsmall = 1) %>%
    paste0(., "%")

  if (for_table == T) {
    if (sum(count$freq) == 100) {
      perc <- count$freq[count$x == max(count$x)]
    } else {
      cli::cli_abort("`nrep` was not 100!")
    }
  }

  # Return
  return(perc)
}

#' Sample class assignment
#'
#' @param lca An object of class `poLCA` created by `poLCA::poLCA()`
#' @param ndraws The number of random draws of latent class assignment
#'
#' @returns A data.frame with `ndraws` variables representing one random draw of
#' latent class assignments each. Variables are factors with labels `lc1`,
#' `lc2`, etc.
#' @export
#'
#' @examples
#' f <- cbind(var1, var2, var3, var4) ~ 1
#' lca <- poLCA::poLCA(f, lca_example_data, nclass = 3, verbose = FALSE)
#' df <- cbind(lca_example_data, lca_sample(lca))
lca_sample <- function(lca, ndraws = 20) {
  # Check inputs
  checkmate::assert_class(lca, classes = "poLCA", null.ok = FALSE)
  checkmate::assert_integerish(ndraws,
    lower = 1, null.ok = FALSE, len = 1, any.missing = FALSE
  )

  # Number of classes
  nclass <- lca$call$nclass

  # Draw values
  lca$posterior %>%
    t() %>%
    as.data.frame() %>%
    purrr::map(\(x) sample(1:nclass,
      size = ndraws, replace = TRUE, prob = x
    )) %>%
    purrr::map(as.data.frame) %>%
    purrr::list_cbind() %>%
    t() %>%
    as.data.frame() %>%
    magrittr::set_colnames(paste0("draws", 1:ndraws)) %>%
    magrittr::set_rownames(1:nrow(.)) %>%
    dplyr::mutate(
      dplyr::across(
        tidyselect::everything(),
        \(x) factor(x, levels = 1:nclass, labels = paste0("lc", 1:nclass))
      )
    )
}

# # Get descriptive table by latent class for single categorical variable
# lc_desc_cat <- function(df, var, cat_to_leave_out, n_draws = 20) {
#   # Check input
#   checkmate::assert_character()
#
#   if (var %>% is_in(df %>% colnames()) %>% not()) {
#     stop("fun_lc_desc: Attribute 'var' is not a variable in 'df'!")
#   }
#
#   if (df[, var] %>% is.factor() %>% not()) {
#     stop("fun_lc_desc: Attribute 'var' must be a factor!")
#   }
#
#   if (missing(cat_to_leave_out) %>% not()) {
#     if (cat_to_leave_out %>% is_in(df[, var] %>% levels()) %>% not()) {
#       stop("fun_lc_desc: Attribute 'cat_to_leave_out' is not a level of 'var'!")
#     }
#   }
#
#   # Get table
#   if (missing(cat_to_leave_out)) {
#     tab_levels <- df[, var] %>% levels()
#   } else {
#     tab_levels <- df[, var] %>%
#       levels() %>%
#       magrittr::extract(df[, var] %>%
#         levels() %>%
#         magrittr::equals(cat_to_leave_out) %>%
#         magrittr::not())
#   }
#
#   tab <- matrix(rep(NA, length(tab_levels) * 9),
#     nrow = length(tab_levels), ncol = 9
#   ) %>%
#     as.data.frame() %>%
#     magrittr::set_colnames(c(
#       "variable", "level", "missings", "all", "class1",
#       "class2", "class3", "class4", "class5"
#     ))
#
#   tab$variable[1] <- var
#   tab$level <- tab_levels
#   tab$missings[1] <- paste0(
#     df[, var] %>%
#       is.na() %>%
#       sum(na.rm = T),
#     " (",
#     df[, var] %>%
#       is.na() %>%
#       sum(na.rm = T) %>%
#       magrittr::divide_by(df %>% nrow()) %>%
#       magrittr::multiply_by(100) %>%
#       round(1) %>%
#       format(nsmall = 1),
#     ")"
#   )
#
#   for (i in 1:length(tab_levels)) {
#     tab$all[i] <- paste0(
#       df[, var] %>%
#         magrittr::equals(tab$level[i]) %>%
#         sum(na.rm = T),
#       " (",
#       df[, var] %>%
#         magrittr::equals(tab$level[i]) %>%
#         sum(na.rm = T) %>%
#         magrittr::divide_by(df[, var] %>%
#           is.na() %>%
#           magrittr::not() %>%
#           sum(na.rm = T)) %>%
#         magrittr::multiply_by(100) %>%
#         round(1) %>%
#         format(nsmall = 1),
#       ")"
#     )
#   }
#   rm(i)
#
#   n_classes <- df$draws1 %>%
#     levels() %>%
#     length()
#
#   for (k in 1:n_classes) {
#     rel_temp <- matrix(rep(NA, n_draws * length(tab_levels)),
#       nrow = n_draws, ncol = length(tab_levels)
#     ) %>%
#       as.data.frame() %>%
#       magrittr::set_colnames(tab_levels)
#
#     for (i in 1:n_draws) {
#       df_class <- df[df[, paste0("draws", i)] == paste0("class", k), ]
#
#       for (j in 1:length(tab_levels)) {
#         rel_temp[i, tab_levels[j]] <- df_class[, var] %>%
#           magrittr::equals(tab_levels[j]) %>%
#           sum(na.rm = T) %>%
#           magrittr::divide_by(df_class[, var] %>%
#             is.na() %>%
#             magrittr::not() %>%
#             sum(na.rm = T)) %>%
#           magrittr::multiply_by(100)
#       }
#       rm(j)
#     }
#     rm(i)
#
#     for (i in 1:length(tab_levels)) {
#       tab[i, paste0("class", k)] <- paste0(
#         rel_temp[, tab$level[i]] %>%
#           mean() %>%
#           round(1) %>%
#           format(nsmall = 1),
#         " (",
#         rel_temp[, tab$level[i]] %>%
#           quantile(probs = c(0.05)) %>%
#           round(1) %>%
#           format(nsmall = 1),
#         "-",
#         rel_temp[, tab$level[i]] %>%
#           quantile(probs = c(0.95)) %>%
#           round(1) %>%
#           format(nsmall = 1),
#         ")"
#       )
#     }
#     rm(i)
#   }
#   rm(k)
#
#   # Return
#   return(tab)
# }

# # Get descriptive table by latent class for single numerical variable
# lc_desc_num <- function(lca, df, var, unit = "(add unit)", n_draws = 20) {
#   # Check input
#
#   if (var %>% is_in(df %>% colnames()) %>% not()) {
#     stop("fun_lc_desc_num: Attribute 'var' is not a variable in 'df'!")
#   }
#
#   if (df[, var] %>% is.numeric() %>% not()) {
#     stop("fun_lc_desc_num: Attribute 'var' must be numeric!")
#   }
#
#   if (unit %>% is.character() %>% not()) {
#     stop("fun_lc_desc_num: Attribute 'unit' must be a character!")
#   }
#
#   # Get number of classes
#   # nclass <- lca3[["call"]][["nclass"]]
#
#
#   # Get table
#   tab <- matrix(rep("", 1 * 9), nrow = 1, ncol = 9) %>%
#     as.data.frame() %>%
#     magrittr::set_colnames(c(
#       "variable", "level", "missings", "all",
#       "class1", "class2", "class3", "class4", "class5"
#     ))
#
#   tab[1, 1] <- var
#   tab[1, 2] <- unit
#   # Missings
#   tab[1, 3] <- paste0(
#     df[, var] %>%
#       is.na() %>%
#       sum(na.rm = T),
#     " (",
#     df[, var] %>%
#       is.na() %>%
#       sum(na.rm = T) %>%
#       magrittr::divide_by(df %>% nrow()) %>%
#       magrittr::multiply_by(100) %>%
#       round(1) %>%
#       format(nsmall = 1),
#     ")"
#   )
#
#   # Mean (SD) for all classes together
#   tab[1, 4] <- paste0(
#     df[, var] %>%
#       mean(na.rm = T) %>%
#       round(1) %>%
#       format(nsmall = 1),
#     " (",
#     df[, var] %>%
#       sd(na.rm = T) %>%
#       round(1) %>%
#       format(nsmall = 1),
#     ")"
#   )
#
#   for (k in 1:5) {
#     rel_temp <- matrix(rep(NA, n_draws * 2), nrow = n_draws, ncol = 2) %>%
#       as.data.frame() %>%
#       magrittr::set_colnames(c("mean", "sd"))
#
#     for (i in 1:n_draws) {
#       df_class <- df[df[, paste0("draws", i)] == paste0("class", k), ]
#
#       rel_temp[i, "mean"] <- df_class[, var] %>% mean(na.rm = T)
#       rel_temp[i, "sd"] <- df_class[, var] %>% sd(na.rm = T)
#     }
#     rm(i)
#
#     tab[1, paste0("class", k)] <- paste0(
#       rel_temp$mean %>%
#         mean() %>%
#         round(1) %>%
#         format(nsmall = 1),
#       " (",
#       rel_temp$mean %>%
#         quantile(probs = c(0.05)) %>%
#         round(1) %>%
#         format(nsmall = 1),
#       "-",
#       rel_temp$mean %>%
#         quantile(probs = c(0.95)) %>%
#         round(1) %>%
#         format(nsmall = 1),
#       ") ",
#       rel_temp$sd %>%
#         mean() %>%
#         round(1) %>%
#         format(nsmall = 1),
#       " (",
#       rel_temp$sd %>%
#         quantile(probs = c(0.05)) %>%
#         round(1) %>%
#         format(nsmall = 1),
#       "-",
#       rel_temp$sd %>%
#         quantile(probs = c(0.95)) %>%
#         round(1) %>%
#         format(nsmall = 1),
#       ")"
#     )
#   }
#   rm(k)
#
#   # Return
#   return(tab)
# }


# # Fitting LCA models with multiply imputed data
# n_draws <- 20
# list_est <- vector(mode = "list", length = length(df_imp))
# list_p <- vector(mode = "list", length = length(df_imp))
# list_entropy <- vector(mode = "numeric", length = length(df_imp))
#
# for (j in 1:length(df_imp)) {
#   lca_temp <- poLCA(f_s2s3cleandisinf, df_imp[[j]], nclass = 5, na.rm = F, probs.start = probs.start.mi, verbose = F)
#
#   ## Entropy
#   list_entropy[j] <- poLCA.entropy(lca_temp)
#
#   ## Draws
#   draws <- matrix(rep(NA, n_draws * 1143), nrow = 1143, ncol = n_draws) %>%
#     as.data.frame() %>%
#     set_colnames(paste0("draws", c(1:n_draws)))
#
#   set.seed(5654112 + j)
#
#   for (i in 1:nrow(draws)) {
#     draws[i, ] <- sample(c(1:5), size = n_draws, replace = T, prob = lca_temp$posterior[i, ])
#   }
#   rm(i)
#
#   df_imp[[j]] %<>% cbind(., draws)
#   rm(draws)
#
#   df_imp[[j]]$draws1 %<>% factor(levels = c(1:5), labels = c("class1", "class2", "class3", "class4", "class5"))
#   df_imp[[j]]$draws2 %<>% factor(levels = c(1:5), labels = c("class1", "class2", "class3", "class4", "class5"))
#   df_imp[[j]]$draws3 %<>% factor(levels = c(1:5), labels = c("class1", "class2", "class3", "class4", "class5"))
#   df_imp[[j]]$draws4 %<>% factor(levels = c(1:5), labels = c("class1", "class2", "class3", "class4", "class5"))
#   df_imp[[j]]$draws5 %<>% factor(levels = c(1:5), labels = c("class1", "class2", "class3", "class4", "class5"))
#   df_imp[[j]]$draws6 %<>% factor(levels = c(1:5), labels = c("class1", "class2", "class3", "class4", "class5"))
#   df_imp[[j]]$draws7 %<>% factor(levels = c(1:5), labels = c("class1", "class2", "class3", "class4", "class5"))
#   df_imp[[j]]$draws8 %<>% factor(levels = c(1:5), labels = c("class1", "class2", "class3", "class4", "class5"))
#   df_imp[[j]]$draws9 %<>% factor(levels = c(1:5), labels = c("class1", "class2", "class3", "class4", "class5"))
#   df_imp[[j]]$draws10 %<>% factor(levels = c(1:5), labels = c("class1", "class2", "class3", "class4", "class5"))
#   df_imp[[j]]$draws11 %<>% factor(levels = c(1:5), labels = c("class1", "class2", "class3", "class4", "class5"))
#   df_imp[[j]]$draws12 %<>% factor(levels = c(1:5), labels = c("class1", "class2", "class3", "class4", "class5"))
#   df_imp[[j]]$draws13 %<>% factor(levels = c(1:5), labels = c("class1", "class2", "class3", "class4", "class5"))
#   df_imp[[j]]$draws14 %<>% factor(levels = c(1:5), labels = c("class1", "class2", "class3", "class4", "class5"))
#   df_imp[[j]]$draws15 %<>% factor(levels = c(1:5), labels = c("class1", "class2", "class3", "class4", "class5"))
#   df_imp[[j]]$draws16 %<>% factor(levels = c(1:5), labels = c("class1", "class2", "class3", "class4", "class5"))
#   df_imp[[j]]$draws17 %<>% factor(levels = c(1:5), labels = c("class1", "class2", "class3", "class4", "class5"))
#   df_imp[[j]]$draws18 %<>% factor(levels = c(1:5), labels = c("class1", "class2", "class3", "class4", "class5"))
#   df_imp[[j]]$draws19 %<>% factor(levels = c(1:5), labels = c("class1", "class2", "class3", "class4", "class5"))
#   df_imp[[j]]$draws20 %<>% factor(levels = c(1:5), labels = c("class1", "class2", "class3", "class4", "class5"))
#
#
#   ## Extracting indicator estimates
#
#   nclass <- lca_temp$call$nclass
#
#   df_est <- expand.grid(c("s2", "s3"), c("spray_cleaning_2c", "spray_disinf_2c", "method_disinf_2c"), paste0("class", c(1:nclass))) %>%
#     set_colnames(c("study", "var", "class"))
#
#   df_est$est <- NA
#   df_est$se <- NA
#
#   for (i in 1:nrow(df_est)) {
#     class_number <- df_est$class[i] %>%
#       as.character() %>%
#       str_replace("class", "") %>%
#       as.integer()
#     var_temp <- paste0(df_est$study[i], "_", df_est$var[i])
#
#     df_est[i, c("est", "se")] <- fun_lca_ci(
#       df = df_imp[[j]], lca = lca_temp,
#       var = var_temp, class = class_number, cat = "weekly", nice = F, out_se = T
#     ) %>% round(5)
#   }
#   rm(i, class_number, var_temp)
#
#   list_est[[j]] <- df_est
#
#   ## Extracting prevalence estimates
#
#   df_p <- expand.grid(paste0("class", c(1:nclass))) %>%
#     set_colnames(c("class"))
#
#   df_p$est <- NA
#   df_p$se <- NA
#
#   for (i in 1:nrow(df_p)) {
#     class_number <- df_p$class[i] %>%
#       as.character() %>%
#       str_replace("class", "") %>%
#       as.integer()
#
#     df_p[i, c("est", "se")] <- fun_lca_ci_p(df = df_imp[[j]], lca = lca_temp, class = class_number, nice = F, out_se = T) %>% round(5)
#   }
#   rm(i, class_number)
#
#   list_p[[j]] <- df_p
# }
# rm(j, df_est, df_p, lca_temp)
#
#
# # Pool
#
# ## indicators
#
# df_est_pooled <- expand.grid(c("s2", "s3"), c("spray_cleaning_2c", "spray_disinf_2c", "method_disinf_2c"), paste0("class", c(1:nclass))) %>%
#   set_colnames(c("study", "var", "class"))
#
# df_est_pooled$est <- NA
# df_est_pooled$se <- NA
#
# for (i in 1:nrow(df_est_pooled)) {
#   ## empty containers
#   est_temp <- vector(mode = "numeric", length = length(list_est))
#   se_temp <- vector(mode = "numeric", length = length(list_est))
#
#   ## fill container of point estimates
#   for (j in 1:length(list_est)) {
#     est_temp[j] <- list_est[[j]][list_est[[j]]$study == df_est_pooled$study[i] &
#       list_est[[j]]$var == df_est_pooled$var[i] &
#       list_est[[j]]$class == df_est_pooled$class[i], "est"]
#   }
#   rm(j)
#
#   ## pool point estimates
#   df_est_pooled$est[i] <- est_temp %>% mean()
#
#   ## fill empty container of standard errors
#   for (j in 1:length(list_est)) {
#     se_temp[j] <- list_est[[j]][list_est[[j]]$study == df_est_pooled$study[i] &
#       list_est[[j]]$var == df_est_pooled$var[i] &
#       list_est[[j]]$class == df_est_pooled$class[i], "se"]
#   }
#   rm(j)
#
#   ## pool standard errors
#   vw <- se_temp %>%
#     raise_to_power(2) %>%
#     mean()
#   vb <- est_temp %>%
#     subtract(est_temp %>% mean()) %>%
#     raise_to_power(2) %>%
#     sum() %>%
#     divide_by(length(df_imp) - 1)
#
#   df_est_pooled$se[i] <- sum(vw, vb, vb %>% divide_by(length(df_imp))) %>% sqrt()
# }
# rm(i)
#
# ## prevalences
#
# df_p_pooled <- expand.grid(paste0("class", c(1:nclass))) %>%
#   set_colnames(c("class"))
#
# df_p_pooled$est <- NA
# df_p_pooled$se <- NA
#
# for (i in 1:nrow(df_p_pooled)) {
#   ## empty containers
#   est_temp <- vector(mode = "numeric", length = length(list_p))
#   se_temp <- vector(mode = "numeric", length = length(list_p))
#
#   ## fill container of point estimates
#   for (j in 1:length(list_p)) {
#     est_temp[j] <- list_p[[j]][list_p[[j]]$class == df_p_pooled$class[i], "est"]
#   }
#   rm(j)
#
#   ## pool point estimates
#   df_p_pooled$est[i] <- est_temp %>% mean()
#
#   ## fill empty container of standard errors
#   for (j in 1:length(list_p)) {
#     se_temp[j] <- list_p[[j]][list_p[[j]]$class == df_p_pooled$class[i], "se"]
#   }
#   rm(j)
#
#   ## pool standard errors
#   vw <- se_temp %>%
#     raise_to_power(2) %>%
#     mean()
#   vb <- est_temp %>%
#     subtract(est_temp %>% mean()) %>%
#     raise_to_power(2) %>%
#     sum() %>%
#     divide_by(length(df_imp) - 1)
#
#   df_p_pooled$se[i] <- sum(vw, vb, vb %>% divide_by(length(df_imp))) %>% sqrt()
# }
# rm(i, est_temp, se_temp, n_draws, nclass, vb, vw, list_est, list_p)
#
# # Create prevalence labels
#
# df_p_pooled$lower <- df_p_pooled$est - 1.96 * df_p_pooled$se
# df_p_pooled$upper <- df_p_pooled$est + 1.96 * df_p_pooled$se
#
# df_p_pooled$lower[df_p_pooled$lower < 0] <- 0
# df_p_pooled$upper[df_p_pooled$upper > 1] <- 1
#
#
# df_p_pooled$label <- paste0(
#   df_p_pooled$est %>% multiply_by(100) %>% round(1) %>% format(nsmall = 1) %>% str_trim(side = "both"),
#   "% [95% CI: ",
#   df_p_pooled$lower %>% multiply_by(100) %>% round(1) %>% format(nsmall = 1) %>% str_trim(side = "both"),
#   "%-",
#   df_p_pooled$upper %>% multiply_by(100) %>% round(1) %>% format(nsmall = 1) %>% str_trim(side = "both"),
#   "%]"
# )



# Helper functions --------------------------------------------------------

#' Extract latent class model description
#'
#' @param lca An object of class `poLCA` created by `poLCA::poLCA()`
#'
#' @returns A named numeric vector holding information that describes the model.
#'
#' @noRd
lca_extract <- function(lca) {
  # Check inputs
  checkmate::assert_class(lca, classes = "poLCA", null.ok = FALSE)

  # Extract
  out <- vector(mode = "numeric", length = 10)
  names(out) <- c(
    "nclass", "nparam", "N", "Nobs", "Gsq", "llik", "df", "AIC", "BIC", "ident"
  )

  out["nclass"] <- lca$call$nclass
  out["nparam"] <- lca$npar
  out["N"] <- lca$N
  out["Nobs"] <- lca$Nobs
  out["Gsq"] <- lca$Gsq
  out["llik"] <- lca$llik
  out["df"] <- lca$resid.df
  out["AIC"] <- lca$aic
  out["BIC"] <- lca$bic
  out["ident"] <- lca_ident(lca = lca, for_table = T)

  # Return
  return(out)
}

#' Creates output for indicator variables
#'
#' @param lca An object of class `poLCA` created by `poLCA::poLCA()`
#' @param var A sinlge string holding the name of a indicator variable
#' @param class A sinlge integer holding the class number of interest
#' @param cat A single value, either integer holding the category of interest
#' of the variable, or character holding the name of the category
#' @param cut TRUE or FALSE indicating if values below 0 or above 1 in the
#' confidence interval (CI) should be replaced by 0 and 1
#' @param nice TRUE or FALSE indicating if estimate and CI should be formatted
#' nicely
#' @param out_se TRUE or FALSE indicating if standard error should be returned
#' instead of the CI
#'
#' @returns A single string if `nice = TRUE`, a numeric vector of length 3 with
#' estimate,  lower, and upper CI bound if `nice = FALSE`, or a numeric vector
#' of length 2 with estimate and standard error if `out_se = TRUE`
#'
#' @noRd
lca_ci <- function(lca, var, class, cat, cut = FALSE, nice = TRUE,
                   out_se = FALSE) {
  # Check input
  checkmate::assert_class(lca, classes = "poLCA", null.ok = FALSE)
  checkmate::assert_int(class,
    na.ok = FALSE, null.ok = FALSE,
    lower = 1, upper = lca$call$nclass
  )
  checkmate::assert_character(var,
    min.chars = 1, len = 1, any.missing = FALSE,
    null.ok = FALSE
  )
  checkmate::assert_subset(var, choices = colnames(lca$y))
  checkmate::assert_factor(lca$y[[var]])

  checkmate::assert_scalar(cat, na.ok = FALSE, null.ok = FALSE)

  if (!is.integer(cat) & !is.character(cat)) {
    cli::cli_abort("`cat` must be either character or integer!",
      .internal = TRUE
    )
  }

  if (is.integer(cat)) {
    checkmate::assert_integer(cat,
      lower = 1, upper = nlevels(lca$y[[var]]),
      any.missing = FALSE, len = 1, null.ok = FALSE
    )
    cat_integer <- cat
    cat_text <- levels(lca$y[[var]])[cat]
  }

  if (is.character(cat)) {
    checkmate::assert_character(cat, min.chars = 1, len = 1)
    checkmate::assert_subset(cat, choices = levels(lca$y[[var]]))
    cat_text <- cat
    cat_integer <- which(levels(lca$y[[var]]) == cat)
  }

  checkmate::assert_flag(cut, na.ok = FALSE, null.ok = FALSE)
  checkmate::assert_flag(nice, na.ok = FALSE, null.ok = FALSE)
  checkmate::assert_flag(out_se, na.ok = FALSE, null.ok = FALSE)

  # Extract
  prob <- lca$probs[[var]][class, cat_integer]

  se <- lca$probs.se[[var]][class, cat_integer]

  upper <- prob + 1.96 * se
  lower <- prob - 1.96 * se

  if (cut == T) {
    if (upper > 1) {
      upper <- 1
    }

    if (lower < 0) {
      lower <- 0
    }
  }

  if (nice == T) {
    prob %<>% magrittr::multiply_by(100) %>%
      round(1) %>%
      format(nsmall = 1)
    upper %<>% magrittr::multiply_by(100) %>%
      round(1) %>%
      format(nsmall = 1)
    lower %<>% magrittr::multiply_by(100) %>%
      round(1) %>%
      format(nsmall = 1)

    out <- paste0(
      prob,
      "% (",
      lower,
      "-",
      upper,
      "%)"
    )
  }

  if (nice == F) {
    out <- c(prob, lower, upper)
  }

  if (out_se == T) {
    out <- c(prob, se)
  }

  # Return
  return(out)
}

#' Creates output for latent class prevalences
#'
#' @param lca An object of class `poLCA` created by `poLCA::poLCA()`
#' @param class A sinlge integer holding the class number of interest
#' @param cut TRUE or FALSE indicating if values below 0 or above 1 in the
#' confidence interval (CI) should be replaced by 0 and 1
#' @param nice TRUE or FALSE indicating if estimate and CI should be formatted
#' nicely
#' @param out_se TRUE or FALSE indicating if standard error should be returned
#' instead of the CI
#'
#' @returns A single string if `nice = TRUE`, a numeric vector of length 1 if
#' `nice = FALSE`, or a numeric vector of length 2 with estimate and standard
#' error if `out_se = TRUE`
#'
#' @noRd
lca_ci_p <- function(lca, class, cut = FALSE, nice = TRUE, out_se = FALSE) {
  # Check input
  checkmate::assert_class(lca, classes = "poLCA", null.ok = FALSE)
  checkmate::assert_int(class,
    na.ok = FALSE, null.ok = FALSE,
    lower = 1, upper = lca$call$nclass
  )

  checkmate::assert_flag(cut, na.ok = FALSE, null.ok = FALSE)
  checkmate::assert_flag(nice, na.ok = FALSE, null.ok = FALSE)
  checkmate::assert_flag(out_se, na.ok = FALSE, null.ok = FALSE)

  # Extract
  prob <- lca$P[class]
  se <- lca$P.se[class]

  if (nice == T) {
    upper <- prob + 1.96 * se
    lower <- prob - 1.96 * se

    if (cut == T) {
      if (upper > 1) {
        upper <- 1
      }

      if (lower < 0) {
        lower <- 0
      }
    }

    prob %<>% magrittr::multiply_by(100) %>%
      round(1) %>%
      format(nsmall = 1)
    upper %<>% magrittr::multiply_by(100) %>%
      round(1) %>%
      format(nsmall = 1)
    lower %<>% magrittr::multiply_by(100) %>%
      round(1) %>%
      format(nsmall = 1)

    out <- paste0(
      prob,
      "% [95% CI: ",
      lower,
      "%-",
      upper,
      "%]"
    )
  }

  if (nice == F) {
    out <- prob %>% round(3)
  }

  if (out_se == T) {
    out <- c(prob, se)
  }

  # Return
  return(out)
}
