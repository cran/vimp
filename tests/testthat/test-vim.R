# load required functions and packages
library("testthat")
suppressWarnings(library("SuperLearner"))

# generate the data -- note that this is a simple setting, for speed -----------
set.seed(4747)
p <- 3
n <- 5e4
x <- as.data.frame(replicate(p, stats::rnorm(n, 0, 1)))
# apply the function to the covariates
y <- 1 + 0.5 * x[, 1] + 0.75 * x[, 2] + stats::rnorm(n, 0, 1)
true_var <- 1 + .5 ^ 2 + .75 ^ 2
# note that true difference in R-squareds for variable j, under independence, is
# beta_j^2 * var(x_j) / var(y)
r2_one <- 0.5 ^ 2 * 1 / true_var
r2_two <- 0.75 ^ 2 * 1 / true_var
folds <- sample(rep(seq_len(2), length = length(y)))
folds_lst <- lapply(as.list(seq_len(2)), function(i) which(folds == i))

# set up a library for SuperLearner
learners <- c("SL.glm", "SL.mean")
V <- 5

# test VIM ---------------------------------------------------------------------
set.seed(4747)
test_that("VIM without sample splitting works", {
  est <- vim(Y = y, X = x, indx = 2, type = "r_squared", run_regression = TRUE,
               SL.library = learners, alpha = 0.05, cvControl = list(V = V),
               env = environment(), sample_splitting = FALSE)
  # check that the estimate is approximately correct
  expect_equal(est$est, r2_two, tolerance = 0.1, scale = 1)
})
set.seed(4747)
test_that("ANOVA-VIM without sample splitting works", {
  expect_message(
    est <- vim(Y = y, X = x, indx = 2, type = "anova", run_regression = TRUE,
               SL.library = learners, alpha = 0.05, cvControl = list(V = V),
               env = environment(), sample_splitting = FALSE)
  )
  # check that the estimate is approximately correct
  expect_equal(est$est, r2_two, tolerance = 0.1, scale = 1)
})

set.seed(1234)
test_that("General variable importance estimates using internally-computed fitted values work", {
  est <- vim(Y = y, X = x, indx = 2, type = "r_squared", run_regression = TRUE,
                SL.library = learners, alpha = 0.05, cvControl = list(V = V),
             env = environment(), sample_splitting_folds = folds)
  # check that the estimate is approximately correct
  expect_equal(est$est, r2_two, tolerance = 0.1, scale = 1)
  # check that the SE, CI work
  expect_length(est$ci, 2)
  expect_length(est$se, 1)
  # check that the p-value worked
  expect_length(est$p_value, 1)
  expect_true(est$test)
  # check that printing, plotting, etc. work
  expect_silent(format(est)[1])
  expect_output(print(est), "Estimate", fixed = TRUE)
  # check that influence curve worked
  expect_length(est$eif_full, length(y) / 2)
})

# fit nuisance regressions -----------------------------------------------------
# fit the data with all covariates and sample-splitting (note V = 2 in CV.SL)
set.seed(4747)
full_fit <- suppressWarnings(
  SuperLearner::CV.SuperLearner(Y = y, X = x, SL.library = learners,
                                          cvControl = list(V = 2, validRows = folds_lst),
                                          innerCvControl = list(list(V = V)))
)
full_fitted <- SuperLearner::predict.SuperLearner(full_fit, onlySL = TRUE)$pred
# fit the data with only X1
reduced_fit <- suppressWarnings(
  SuperLearner::CV.SuperLearner(Y = full_fitted,
                                             X = x[, -2, drop = FALSE],
                                             SL.library = learners,
                                             cvControl = list(V = 2, validRows = full_fit$folds),
                                             innerCvControl = list(list(V = V)))
)
reduced_fitted <- SuperLearner::predict.SuperLearner(reduced_fit, onlySL = TRUE)$pred

# test VIM with pre-computed nuisance estimates --------------------------------
test_that("General variable importance estimates using externally-computed fitted values work", {
    # provide folds
    est <- vim(Y = y, X = x, indx = 2, type = "r_squared",
               run_regression = FALSE,
               f1 = full_fitted, f2 = reduced_fitted,
               sample_splitting_folds = folds)
    # check that estimate worked
    expect_equal(est$est, r2_two, tolerance = 0.15, scale = 1)
    # check that p-value exists
    expect_length(est$p_value, 1)
    # check that CIs with other transformations work
    ci_log <- vimp_ci(est$est, est$se, scale = "log", level = 0.95)
    ci_logit <- vimp_ci(est$est, est$se, scale = "logit", level = 0.95)
    expect_length(ci_log, 2)
    expect_length(ci_logit, 2)
})
test_that("VIM estimates using externally-computed fitted values with different folds for estimation and inference work", {
  # provide folds
  est_average <- vim(Y = y, X = x, indx = 2, type = "r_squared",
                     run_regression = FALSE,
                     f1 = full_fitted, f2 = reduced_fitted,
                     sample_splitting_folds = folds,
                     final_point_estimate = "average")
  est_full <- vim(Y = y, X = x, indx = 2, type = "r_squared",
                  run_regression = FALSE,
                  f1 = full_fitted, f2 = reduced_fitted,
                  sample_splitting_folds = folds,
                  final_point_estimate = "full")
  # check that estimate worked
  expect_equal(est_average$est, r2_two, tolerance = 0.15, scale = 1)
  expect_equal(est_full$est, r2_two, tolerance = 0.15, scale = 1)
})


# measures of predictiveness ---------------------------------------------------
test_that("Measures of predictiveness work", {
  full_rsquared <- est_predictiveness(full_fitted, y,
                                      type = "r_squared")
  expect_equal(full_rsquared$point_est, 0.44, tolerance = 0.1)
  expect_length(full_rsquared$eif, length(y))
})

# error messages ---------------------------------------------------------------
test_that("Error messages work", {
    expect_error(vim(X = x))
    expect_error(vim(Y = y))
    expect_error(vim(Y = y, X = x, SL.library = NULL))
    expect_error(vim(Y = y, X = x, run_regression = FALSE))
    expect_error(vim(Y = y, f1 = mean(y)))
    expect_error(vim(Y = y, f1 = rep(mean(y), length(y)), f2 = mean(y)))
    expect_error(vim(Y = y, X = as.data.frame(x), SL.library = learners,
                     type = "nonsense_type"))
    expect_error(vim(Y = y, X = X, SL.library = learners, indx = ncol(X) + 1))
})
