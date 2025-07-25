## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message = FALSE---------------------------------------------------
library("vimp")
library("SuperLearner")

## ----load-vrc01-data----------------------------------------------------------
# read in the data
data("vrc01")
# subset to the columns of interest for this analysis
library("dplyr")
library("tidyselect")
# retain only the columns of interest for this analysis
y <- vrc01$ic50.censored
X <- vrc01 %>%
  select(starts_with("geog"), starts_with("subtype"), starts_with("length"))
set.seed(1234)
vrc01_folds <- make_folds(y = y, V = 2)

## ----est-regressions-lm, warning = FALSE--------------------------------------
library("rlang")
vrc01_subset <- vrc01 %>%
  select(ic50.censored, starts_with("geog"), starts_with("subtype"), starts_with("length")) %>%
  rename(y = ic50.censored)
# estimate prediction function on each subset, predict on held-out fold
full_fit <- vector("numeric", length = nrow(vrc01))
for (v in 1:2) {
  train_v <- subset(vrc01_subset, vrc01_folds != v)
  test_v <- subset(vrc01_subset, vrc01_folds == v)
  full_mod <- glm(y ~ ., data = train_v)
  full_fit[vrc01_folds == v] <- predict(full_mod, newdata = test_v)
}

# estimate the reduced conditional means for each of the individual variables
# remove the outcome for the predictor matrix
geog_indx <- max(which(grepl("geog", names(X))))
for (i in seq_len(ncol(X) - geog_indx)) {
  this_name <- names(X)[i + geog_indx]
  red_fit <- vector("numeric", length = nrow(vrc01))
  for (v in 1:2) {
    train_v <- subset(vrc01_subset, vrc01_folds != v)
    test_v <- subset(vrc01_subset, vrc01_folds == v)
    red_fit[vrc01_folds == v] <- suppressWarnings(
      predict(glm(y ~ .,  data = train_v %>% select(-!!this_name)), 
              newdata = test_v)  
    )
  }
  this_vim <- vim(Y = y, f1 = full_fit, f2 = red_fit, indx = i + geog_indx,
                  run_regression = FALSE, type = "r_squared",
                  sample_splitting_folds = vrc01_folds, scale = "logit")
  if (i == 1) {
    lm_mat <- this_vim
  } else {
    lm_mat <- merge_vim(lm_mat, this_vim)
  }
}
# print out the matrix
lm_mat

## ----estimate-full-regression-with-cf, message = FALSE, warning = FALSE-------
learners <- "SL.ranger"
# estimate the full regression function
V <- 2
set.seed(4747)
full_cv_fit <- suppressWarnings(
  SuperLearner::CV.SuperLearner(
  Y = y, X = X, SL.library = learners, cvControl = list(V = 2 * V),
  innerCvControl = list(list(V = V)), family = binomial()
)
)
# get a numeric vector of cross-fitting folds
cross_fitting_folds <- get_cv_sl_folds(full_cv_fit$folds)
# get sample splitting folds
set.seed(1234)
sample_splitting_folds <- make_folds(unique(cross_fitting_folds), V = 2)
full_cv_preds <- full_cv_fit$SL.predict

## ----estimate-reduced-regressions-with-cf, message = FALSE, warning = FALSE----
vars <- names(X)[(geog_indx + 1):ncol(X)]
set.seed(1234)
for (i in seq_len(length(vars))) {
  # use "eval" and "parse" to assign the objects of interest to avoid duplicating code
  eval(parse(text = paste0("reduced_", vars[i], "_cv_fit <- suppressWarnings(SuperLearner::CV.SuperLearner(
  Y = y, X = X[, -(geog_indx + i), drop = FALSE], SL.library = learners,
  cvControl = SuperLearner::SuperLearner.CV.control(V = 2 * V, validRows = full_cv_fit$folds),
  innerCvControl = list(list(V = V)), family = binomial()
))")))
  eval(parse(text = paste0("reduced_", vars[i], "_cv_preds <- reduced_", vars[i], "_cv_fit$SL.predict")))
}

## ----cf-vims------------------------------------------------------------------
for (i in seq_len(length(vars))) {
  # again, use "eval" and "parse" to assign the objects of interest to avoid duplicating code
  eval(parse(text = paste0("cf_", vars[i], "_vim <- vimp_rsquared(Y = y,
  cross_fitted_f1 = full_cv_preds, cross_fitted_f2 = reduced_", vars[i], "_cv_preds,
  indx = (geog_indx + i), cross_fitting_folds = cross_fitting_folds,
  sample_splitting_folds = sample_splitting_folds, run_regression = FALSE, alpha = 0.05,
  V = V, na.rm = TRUE, scale = 'logit')")))
}
cf_ests <- merge_vim(cf_subtype.is.01_AE_vim,
                     cf_subtype.is.02_AG_vim, cf_subtype.is.07_BC_vim,
                     cf_subtype.is.A1_vim, cf_subtype.is.A1C_vim,
                     cf_subtype.is.A1D_vim, cf_subtype.is.B_vim,
                     cf_subtype.is.C_vim, cf_subtype.is.D_vim,
                     cf_subtype.is.O_vim, cf_subtype.is.Other_vim,
                     cf_length.env_vim, cf_length.gp120_vim, cf_length.loop.e_vim,
                     cf_length.loop.e.outliers_vim, cf_length.v5_vim,
                     cf_length.v5.outliers_vim)
all_vars <- c(paste0("Subtype is ", c("01_AE", "02_AG", "07_BC", "A1", "A1C", "A1D",
                                      "B", "C", "D", "O", "Other")),
              paste0("Length of ", c("Env", "gp120", "V5", "V5 outliers", "Loop E",
                                     "Loop E outliers")))

## ----plot-cf-vim, fig.width = 8.5, fig.height = 8-----------------------------
library("ggplot2")
library("cowplot")
theme_set(theme_cowplot())
cf_est_plot_tib <- cf_ests$mat %>%
  mutate(
    var_fct = rev(factor(s, levels = cf_ests$mat$s,
                     labels = all_vars[as.numeric(cf_ests$mat$s) - geog_indx],
                     ordered = TRUE))
  )

# plot
cf_est_plot_tib %>%
  ggplot(aes(x = est, y = var_fct)) +
  geom_point() +
  geom_errorbarh(aes(xmin = cil, xmax = ciu)) +
  xlab(expression(paste("Variable importance estimates: ", R^2, sep = ""))) +
  ylab("") +
  ggtitle("Estimated individual feature importance") +
  labs(subtitle = "in the VRC01 data (considering only geographic confounders, subtype, and viral geometry)")

## ----cf-group-vim, fig.width = 8.5, fig.height = 8, warning = FALSE-----------
set.seed(91011)
reduced_subtype_cv_fit <- suppressWarnings(
  SuperLearner::CV.SuperLearner(
  Y = y, X = X[, -c(5:15), drop = FALSE], SL.library = learners,
  cvControl = SuperLearner::SuperLearner.CV.control(V = 2 * V, validRows = full_cv_fit$folds),
  innerCvControl = list(list(V = V)), family = binomial()
)
)
reduced_subtype_cv_preds <- reduced_subtype_cv_fit$SL.predict
reduced_geometry_cv_fit <- suppressWarnings(
  SuperLearner::CV.SuperLearner(
  Y = y, X = X[, -c(16:21), drop = FALSE], SL.library = learners,
  cvControl = SuperLearner::SuperLearner.CV.control(V = 2 * V, validRows = full_cv_fit$folds),
  innerCvControl = list(list(V = V)), family = binomial()
)
)
reduced_geometry_cv_preds <- reduced_geometry_cv_fit$SL.predict
cf_subtype_vim <- vimp_rsquared(
  Y = y, cross_fitted_f1 = full_cv_preds, cross_fitted_f2 = reduced_subtype_cv_preds,
  indx = 5:15, run_regression = FALSE, V = V,
  cross_fitting_folds = cross_fitting_folds, sample_splitting_folds = sample_splitting_folds,
  scale = "logit"
)
cf_geometry_vim <- vimp_rsquared(
  Y = y, cross_fitted_f1 = full_cv_preds, cross_fitted_f2 = reduced_geometry_cv_preds,
  indx = 16:21, run_regression = FALSE, V = V,
  cross_fitting_folds = cross_fitting_folds, sample_splitting_folds = sample_splitting_folds,
  scale = "logit"
)
cf_groups <- merge_vim(cf_subtype_vim, cf_geometry_vim)
all_grp_nms <- c("Viral subtype", "Viral geometry")

grp_plot_tib <- cf_groups$mat %>%
  mutate(
    grp_fct = factor(case_when(
      s == "5,6,7,8,9,10,11,12,13,14,15" ~ "1",
      s == "16,17,18,19,20,21" ~ "2"
    ), levels = c("1", "2"),  labels = all_grp_nms, ordered = TRUE)
  )
grp_plot_tib %>%
  ggplot(aes(x = est, y = grp_fct)) +
  geom_point() +
  geom_errorbarh(aes(xmin = cil, xmax = ciu)) +
  xlab(expression(paste("Variable importance estimates: ", R^2, sep = ""))) +
  ylab("") +
  ggtitle("Estimated feature group importance") +
  labs(subtitle = "in the VRC01 data (considering only geographic confounders, subtype, and viral geometry)")

