## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message = FALSE---------------------------------------------------
library("vimp")
library("SuperLearner")

## ----load-heart-data----------------------------------------------------------
# read in the data from the Elements website
library("RCurl")
heart_data <- read.csv(text = getURL("http://web.stanford.edu/~hastie/ElemStatLearn/datasets/SAheart.data"), header = TRUE, stringsAsFactors = FALSE)
# minor data cleaning
heart <- heart_data[, 2:dim(heart_data)[2]]
heart$famhist <- ifelse(heart$famhist == "Present", 1, 0)
# sample-splitting folds for hypothesis testing
heart_folds <- sample(rep(seq_len(2), length = dim(heart)[1]))

## ----est-heart-regressions-lm-------------------------------------------------
heart_1 <- subset(heart, heart_folds == 1)
full_mod <- lm(chd ~ ., data = heart_1)
full_fit <- predict(full_mod)

# estimate the reduced conditional means for each of the individual variables
# remove the outcome for the predictor matrix
X <- heart[, -dim(heart)[2]]
X_1 <- subset(X, heart_folds == 1)
X_2 <- subset(X, heart_folds == 2)
x_2_mat <- as.matrix(X_2)
# get the full regression fit and use as outcome for reduced regressions; this may provide some stability in this analysis
heart_2 <- subset(heart, heart_folds == 2)
full_fit_2 <- predict(lm(chd ~ ., data = heart_2))
red_mod_sbp <- lm(full_fit ~ x_2_mat[,-1])
red_fit_sbp <- predict(red_mod_sbp)
red_mod_tob <- lm(full_fit ~ x_2_mat[,-2])
red_fit_tob <- predict(red_mod_tob)
red_mod_ldl <- lm(full_fit ~ x_2_mat[,-3])
red_fit_ldl <- predict(red_mod_ldl)
red_mod_adi <- lm(full_fit ~ x_2_mat[,-4])
red_fit_adi <- predict(red_mod_adi)
red_mod_fam <- lm(full_fit ~ x_2_mat[,-5])
red_fit_fam <- predict(red_mod_fam)
red_mod_tpa <- lm(full_fit ~ x_2_mat[, -6])
red_fit_tpa <- predict(red_mod_tpa)
red_mod_obe <- lm(full_fit ~ x_2_mat[,-7])
red_fit_obe <- predict(red_mod_obe)
red_mod_alc <- lm(full_fit ~ x_2_mat[, -8])
red_fit_alc <- predict(red_mod_alc)
red_mod_age <- lm(full_fit ~ x_2_mat[,-9])
red_fit_age <- predict(red_mod_age)

# plug the regression function estimates into vim
lm_vim_sbp <- vim(Y = heart$chd, f1 = full_fit, f2 = red_fit_sbp, indx = 1, run_regression = FALSE, type = "r_squared", sample_splitting_folds = heart_folds)
lm_vim_tob <- vim(Y = heart$chd, f1 = full_fit, f2 = red_fit_tob, indx = 2, run_regression = FALSE, type = "r_squared", sample_splitting_folds = heart_folds)
lm_vim_ldl <- vim(Y = heart$chd, f1 = full_fit, f2 = red_fit_ldl, indx = 3, run_regression = FALSE, type = "r_squared", sample_splitting_folds = heart_folds)
lm_vim_adi <- vim(Y = heart$chd, f1 = full_fit, f2 = red_fit_adi, indx = 4, run_regression = FALSE, type = "r_squared", sample_splitting_folds = heart_folds)
lm_vim_fam <- vim(Y = heart$chd, f1 = full_fit, f2 = red_fit_fam, indx = 5, run_regression = FALSE, type = "r_squared", sample_splitting_folds = heart_folds)
lm_vim_tpa <- vim(Y = heart$chd, f1 = full_fit, f2 = red_fit_tpa, indx = 6, run_regression = FALSE, type = "r_squared", sample_splitting_folds = heart_folds)
lm_vim_obe <- vim(Y = heart$chd, f1 = full_fit, f2 = red_fit_obe, indx = 7, run_regression = FALSE, type = "r_squared", sample_splitting_folds = heart_folds)
lm_vim_alc <- vim(Y = heart$chd, f1 = full_fit, f2 = red_fit_alc, indx = 8, run_regression = FALSE, type = "r_squared", sample_splitting_folds = heart_folds)
lm_vim_age <- vim(Y = heart$chd, f1 = full_fit, f2 = red_fit_age, indx = 9, run_regression = FALSE, type = "r_squared", sample_splitting_folds = heart_folds)

# make a table with the estimates using the merge_vim() function
library("dplyr")
lm_mat <- merge_vim(lm_vim_sbp, lm_vim_tob, lm_vim_ldl, lm_vim_adi,
                lm_vim_fam, lm_vim_tpa, lm_vim_obe, lm_vim_alc, lm_vim_age)
# print out the matrix
lm_mat

## ----vimp-with-sl-fam, message = FALSE----------------------------------------
# small learners library
learners.2 <- c("SL.ranger")

# small number of folds
V <- 2
sl_cvcontrol <- list(V = 2)

# now estimate variable importance
fam_vim <- vim(Y = heart$chd, X = X, indx = 5, SL.library = learners.2, na.rm = TRUE, env = environment(), cvControl = sl_cvcontrol, sample_splitting_folds = heart_folds)

## ----typea-vim, message = FALSE-----------------------------------------------
# specify that full_fit doesn't change; but note that this was fit on the first half of the data
full_fit <- fam_vim$full_fit
# get the full fit on the second half of the data; for stability later
full_fit_2 <- predict(SuperLearner::SuperLearner(Y = heart_2$chd, X = X_2, SL.library = learners.2, cvControl = sl_cvcontrol))$pred

# estimate variable importance for the average number of rooms
start_time <- Sys.time()
reduced_fit <- SuperLearner::SuperLearner(Y = full_fit_2, X = X_2[, -6, drop = FALSE], SL.library = learners.2, cvControl = sl_cvcontrol)
end_time <- Sys.time()
red_fit <- predict(reduced_fit)$pred
tpa_vim <- vim(Y = heart$chd, f1 = full_fit_2, f2 = red_fit, indx = 6, run_regression = FALSE, type = "r_squared", sample_splitting_folds = fam_vim$sample_splitting_folds)

tpa_vim

## ----alc-vim------------------------------------------------------------------
# set up the data, removing the columns for alcohol use and chd
x <- heart_2[, -c(8, dim(heart)[2])]

# fit an RF model using SuperLearner
reduced_mod <- SuperLearner(Y = full_fit_2, X = x, SL.library = learners.2, cvControl = sl_cvcontrol)
reduced_fit <- predict(reduced_mod)$pred
# this takes 2 seconds

# estimate variable importance
alc_vim <- vim(Y = heart$chd, f1 = full_fit_2, f2 = reduced_fit, indx = 8, run_regression = FALSE, type = "r_squared", sample_splitting_folds = fam_vim$sample_splitting_folds)

## ----heart-sl-----------------------------------------------------------------
reduced_sbp <- predict(SuperLearner(Y = full_fit_2, X = X_2[, -1], SL.library = learners.2, cvControl = sl_cvcontrol))$pred
sbp_vim <- vim(Y = heart$chd, f1 = full_fit_2, f2 = reduced_sbp,
                indx = 1, run_regression = FALSE, type = "r_squared", sample_splitting_folds = fam_vim$sample_splitting_folds)

reduced_tob <- predict(SuperLearner(Y = full_fit_2, X = X_2[, -2], SL.library = learners.2, cvControl = sl_cvcontrol))$pred
tob_vim <- vim(Y = heart$chd, f1 = full_fit_2, f2 = reduced_tob,
                indx = 2, run_regression = FALSE, type = "r_squared", sample_splitting_folds = fam_vim$sample_splitting_folds)

reduced_ldl <- predict(SuperLearner(Y = full_fit_2, X = X_2[, -3], SL.library = learners.2, cvControl = sl_cvcontrol))$pred
ldl_vim <- vim(Y = heart$chd, f1 = full_fit_2, f2 = reduced_ldl,
                indx = 3, run_regression = FALSE, type = "r_squared", sample_splitting_folds = fam_vim$sample_splitting_folds)

reduced_adi <- predict(SuperLearner(Y = full_fit_2, X = X_2[, -4], SL.library = learners.2, cvControl = sl_cvcontrol))$pred
adi_vim <- vim(Y = heart$chd, f1 = full_fit_2, f2 = reduced_adi,
                indx = 4, run_regression = FALSE, type = "r_squared", sample_splitting_folds = fam_vim$sample_splitting_folds)

reduced_obe <- predict(SuperLearner(Y = full_fit_2, X = X_2[, -7], SL.library = learners.2, cvControl = sl_cvcontrol))$pred
obe_vim <- vim(Y = heart$chd, f1 = full_fit_2, f2 = reduced_obe,
                indx = 7, run_regression = FALSE, type = "r_squared", sample_splitting_folds = fam_vim$sample_splitting_folds)

reduced_age <- predict(SuperLearner(Y = full_fit_2, X = X_2[, -9], SL.library = learners.2, cvControl = sl_cvcontrol))$pred
age_vim <- vim(Y = heart$chd, f1 = full_fit_2, f2 = reduced_age,
                indx = 9, run_regression = FALSE, type = "r_squared", sample_splitting_folds = fam_vim$sample_splitting_folds)

## ----heart-vim, fig.width = 8.5, fig.height = 8, message = FALSE--------------
library("dplyr")
library("tibble")
library("ggplot2")
library("cowplot")
theme_set(theme_cowplot())
# combine the objects together
ests <- merge_vim(sbp_vim, tob_vim, ldl_vim, adi_vim,
                fam_vim, tpa_vim, obe_vim, alc_vim, age_vim)
all_vars <- c("Sys. blood press.", "Tobacco consump.", "LDL cholesterol",
              "Adiposity", "Family history", "Type A behavior", "Obesity",
              "Alcohol consump.", "Age")

est_plot_tib <- ests$mat %>%
  mutate(
    var_fct = rev(factor(s, levels = ests$mat$s,
                     labels = all_vars[as.numeric(ests$mat$s)],
                     ordered = TRUE))
  )

# plot
est_plot_tib %>%
  ggplot(aes(x = est, y = var_fct)) +
  geom_point() +
  geom_errorbarh(aes(xmin = cil, xmax = ciu)) +
  xlab(expression(paste("Variable importance estimates: ", R^2, sep = ""))) +
  ylab("") +
  ggtitle("Estimated individual feature importance") +
  labs(subtitle = "in the South African heart disease study data")

## ----heart-group-vim, fig.width = 8.5, fig.height = 8-------------------------
# get the estimates
reduced_behav <- predict(SuperLearner(Y = full_fit_2, X = heart_2[, -c(2, 6, 8, dim(heart)[2])], SL.library = learners.2, cvControl = sl_cvcontrol))$pred
behav_vim <- vim(Y = heart$chd, f1 = full_fit_2, f2 = reduced_behav, indx = c(2, 6, 8), run_regression = FALSE, type = "r_squared", sample_splitting_folds = fam_vim$sample_splitting_folds)

reduced_bios <- predict(SuperLearner(Y = full_fit_2, X = heart_2[, -c(1, 3, 4, 5, 7, 9, dim(heart)[2])], SL.library = learners.2, cvControl = sl_cvcontrol))$pred
bios_vim <- vim(Y = heart$chd, f1 = full_fit_2, f2 = reduced_bios, indx = c(1, 3, 4, 5, 7, 9), run_regression = FALSE, type = "r_squared", sample_splitting_folds = fam_vim$sample_splitting_folds)

# combine and plot
groups <- merge_vim(behav_vim, bios_vim)
all_grp_nms <- c("Behavioral features", "Biological features")

grp_plot_tib <- groups$mat %>%
  mutate(
    grp_fct = factor(case_when(
      s == "2,6,8" ~ "1",
      s == "1,3,4,5,7,9" ~ "2"
    ), levels = c("1", "2"),  labels = all_grp_nms, ordered = TRUE)
  )
grp_plot_tib %>%
  ggplot(aes(x = est, y = grp_fct)) +
  geom_point() +
  geom_errorbarh(aes(xmin = cil, xmax = ciu)) +
  xlab(expression(paste("Variable importance estimates: ", R^2, sep = ""))) +
  ylab("") +
  ggtitle("Estimated feature group importance") +
  labs(subtitle = "in the South African heart disease study data")

## ----estimate-full-regression-with-cf, message = FALSE------------------------
x <- heart[, -ncol(heart)]
# estimate the full regression function
V <- 5
set.seed(4747)
full_cv_fit <- SuperLearner::CV.SuperLearner(
  Y = heart$chd, X = x, SL.library = learners.2, cvControl = list(V = 2 * V),
  innerCvControl = list(list(V = V))
)
# get a numeric vector of cross-fitting folds
cross_fitting_folds <- get_cv_sl_folds(full_cv_fit$folds)
# get sample splitting folds
set.seed(1234)
sample_splitting_folds <- make_folds(unique(cross_fitting_folds), V = 2)
# extract the predictions on split portions of the data, for hypothesis testing
full_cv_preds <- extract_sampled_split_predictions(
  cvsl_obj = full_cv_fit, sample_splitting = TRUE,
  sample_splitting_folds = sample_splitting_folds, full = TRUE
)

## ----estimate-redu-regressions-with-cf, message = FALSE-----------------------
vars <- c("sbp", "tob", "ldl", "adi", "fam", "tpa", "obe", "alc", "age")
for (i in seq_len(length(vars))) {
  # use "eval" and "parse" to assign the objects of interest to avoid duplicating code
  eval(parse(text = paste0("reduced_", vars[i], "_cv_fit <- SuperLearner::CV.SuperLearner(
  Y = heart$chd, X = x[, -i, drop = FALSE], SL.library = learners.2,
  cvControl = SuperLearner::SuperLearner.CV.control(V = 2 * V, validRows = full_cv_fit$folds),
  innerCvControl = list(list(V = V))
)")))
  eval(parse(text = paste0("reduced_", vars[i], "_cv_preds <- extract_sampled_split_predictions(
  cvsl_obj = reduced_", vars[i], "_cv_fit, sample_splitting = TRUE,
  sample_splitting_folds = sample_splitting_folds, full = FALSE
)")))
}

## ----cf-vims------------------------------------------------------------------
cf_sbp_vim <- vimp_rsquared(
  Y = heart$chd, cross_fitted_f1 = full_cv_preds, cross_fitted_f2 = reduced_sbp_cv_preds,
  indx = 1, run_regression = FALSE, V = V,
  cross_fitting_folds = cross_fitting_folds, sample_splitting_folds = sample_splitting_folds
)
cf_tob_vim <- vimp_rsquared(
  Y = heart$chd, cross_fitted_f1 = full_cv_preds, cross_fitted_f2 = reduced_tob_cv_preds,
  indx = 2, run_regression = FALSE, V = V,
  cross_fitting_folds = cross_fitting_folds, sample_splitting_folds = sample_splitting_folds
)
cf_ldl_vim <- vimp_rsquared(
  Y = heart$chd, cross_fitted_f1 = full_cv_preds, cross_fitted_f2 = reduced_ldl_cv_preds,
  indx = 3, run_regression = FALSE, V = V,
  cross_fitting_folds = cross_fitting_folds, sample_splitting_folds = sample_splitting_folds
)
cf_adi_vim <- vimp_rsquared(
  Y = heart$chd, cross_fitted_f1 = full_cv_preds, cross_fitted_f2 = reduced_adi_cv_preds,
  indx = 4, run_regression = FALSE, V = V,
  cross_fitting_folds = cross_fitting_folds, sample_splitting_folds = sample_splitting_folds
)
cf_fam_vim <- vimp_rsquared(
  Y = heart$chd, cross_fitted_f1 = full_cv_preds, cross_fitted_f2 = reduced_fam_cv_preds,
  indx = 5, run_regression = FALSE, V = V,
  cross_fitting_folds = cross_fitting_folds, sample_splitting_folds = sample_splitting_folds
)
cf_tpa_vim <- vimp_rsquared(
  Y = heart$chd, cross_fitted_f1 = full_cv_preds, cross_fitted_f2 = reduced_tpa_cv_preds,
  indx = 6, run_regression = FALSE, V = V,
  cross_fitting_folds = cross_fitting_folds, sample_splitting_folds = sample_splitting_folds
)
cf_obe_vim <- vimp_rsquared(
  Y = heart$chd, cross_fitted_f1 = full_cv_preds, cross_fitted_f2 = reduced_obe_cv_preds,
  indx = 7, run_regression = FALSE, V = V,
  cross_fitting_folds = cross_fitting_folds, sample_splitting_folds = sample_splitting_folds
)
cf_alc_vim <- vimp_rsquared(
  Y = heart$chd, cross_fitted_f1 = full_cv_preds, cross_fitted_f2 = reduced_alc_cv_preds,
  indx = 8, run_regression = FALSE, V = V,
  cross_fitting_folds = cross_fitting_folds, sample_splitting_folds = sample_splitting_folds
)
cf_age_vim <- vimp_rsquared(
  Y = heart$chd, cross_fitted_f1 = full_cv_preds, cross_fitted_f2 = reduced_age_cv_preds,
  indx = 9, run_regression = FALSE, V = V,
  cross_fitting_folds = cross_fitting_folds, sample_splitting_folds = sample_splitting_folds
)

cf_ests <- merge_vim(cf_sbp_vim, cf_tob_vim, cf_ldl_vim, cf_adi_vim, cf_fam_vim, cf_tpa_vim, cf_obe_vim, cf_alc_vim, cf_age_vim)

## ----plot-cf-vim, fig.width = 8.5, fig.height = 8-----------------------------
cf_est_plot_tib <- cf_ests$mat %>%
  mutate(
    var_fct = rev(factor(s, levels = cf_ests$mat$s,
                     labels = all_vars[as.numeric(cf_ests$mat$s)],
                     ordered = TRUE))
  )

# plot
cf_est_plot_tib %>%
  ggplot(aes(x = est, y = var_fct)) +
  geom_point() +
  geom_errorbarh(aes(xmin = cil, xmax = ciu)) +
  xlab(expression(paste("Variable importance estimates: ", R^2, sep = ""))) +
  ylab("") +
  ggtitle("Cross-fitted individual feature importance estimates") +
  labs(subtitle = "in the South African heart disease study data")

## ----cf-group-vim, fig.width = 8.5, fig.height = 8----------------------------
reduced_behav_cv_fit <- SuperLearner::CV.SuperLearner(
  Y = heart$chd, X = x[, -c(2, 6, 8), drop = FALSE], SL.library = learners.2,
  cvControl = SuperLearner::SuperLearner.CV.control(V = 2 * V, validRows = full_cv_fit$folds),
  innerCvControl = list(list(V = V))
)
reduced_behav_cv_preds <- extract_sampled_split_predictions(
  cvsl_obj = reduced_behav_cv_fit, sample_splitting = TRUE,
  sample_splitting_folds = sample_splitting_folds, full = FALSE
)
reduced_bios_cv_fit <- SuperLearner::CV.SuperLearner(
  Y = heart$chd, X = x[, -c(1, 3, 4, 5, 7, 9), drop = FALSE], SL.library = learners.2,
  cvControl = SuperLearner::SuperLearner.CV.control(V = 2 * V, validRows = full_cv_fit$folds),
  innerCvControl = list(list(V = V))
)
reduced_bios_cv_preds <- extract_sampled_split_predictions(
  cvsl_obj = reduced_bios_cv_fit, sample_splitting = TRUE,
  sample_splitting_folds = sample_splitting_folds, full = FALSE
)
cf_behav_vim <- vimp_rsquared(
  Y = heart$chd, cross_fitted_f1 = full_cv_preds, cross_fitted_f2 = reduced_behav_cv_preds,
  indx = c(2, 6, 8), run_regression = FALSE, V = V,
  cross_fitting_folds = cross_fitting_folds, sample_splitting_folds = sample_splitting_folds
)
cf_bios_vim <- vimp_rsquared(
  Y = heart$chd, cross_fitted_f1 = full_cv_preds, cross_fitted_f2 = reduced_bios_cv_preds,
  indx = c(1, 3, 4, 5, 7, 9), run_regression = FALSE, V = V,
  cross_fitting_folds = cross_fitting_folds, sample_splitting_folds = sample_splitting_folds
)
cf_groups <- merge_vim(cf_behav_vim, cf_bios_vim)

cf_grp_plot_tib <- cf_groups$mat %>%
  mutate(
    grp_fct = factor(case_when(
      s == "2,6,8" ~ "1",
      s == "1,3,4,5,7,9" ~ "2"
    ), levels = c("1", "2"),  labels = all_grp_nms, ordered = TRUE)
  )
cf_grp_plot_tib %>%
  ggplot(aes(x = est, y = grp_fct)) +
  geom_point() +
  geom_errorbarh(aes(xmin = cil, xmax = ciu)) +
  xlab(expression(paste("Variable importance estimates: ", R^2, sep = ""))) +
  ylab("") +
  ggtitle("Cross-fitted feature group importance estimates") +
  labs(subtitle = "in the South African heart disease study data")

