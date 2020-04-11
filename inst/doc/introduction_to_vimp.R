## ----setup, echo = FALSE, include = FALSE-------------------------------------
library(knitr)
opts_knit$set(cache = FALSE, verbose = TRUE, global.par = TRUE)

## ----more-setup, echo = FALSE-------------------------------------------------
par(mar = c(5, 12, 4, 2) + 0.1)

## ----install-vimp, eval = FALSE-----------------------------------------------
#  install.packages("vimp")

## ----devtools-install-vimp, eval = FALSE--------------------------------------
#  # only run if you don't have devtools
#  # previously installed
#  # install.packages("devtools")
#  devtools::install_github("bdwilliamson/vimp")

## ----load-vimp----------------------------------------------------------------
library("vimp")

## ----gen-data-----------------------------------------------------------------
## -------------------------------------------------------------
## problem setup
## -------------------------------------------------------------
## set up the data
n <- 1000
p <- 2
s <- 1 # desire importance for X_1
x <- data.frame(replicate(p, runif(n, -1, 1)))
y <- (x[,1])^2*(x[,1]+7/5) + (25/9)*(x[,2])^2 + rnorm(n, 0, 1)
## set up folds for hypothesis testing
folds <- sample(rep(seq_len(2), length = length(y)))

## ----learner-lib-small--------------------------------------------------------
library("SuperLearner")
## load specific algorithms
library("gam")
library("xgboost")
SL.xgboost1 <- function(..., max_depth = 1, ntree = 500, shrinkage = 0.1){
  SL.xgboost(..., max_depth = max_depth, ntree = ntree, shrinkage = shrinkage)
}

## ----est-1--------------------------------------------------------------------
est_1 <- vimp_rsquared(Y = y, X = x, indx = 1, run_regression = TRUE,
SL.library = c("SL.xgboost1", "SL.mean"), V = 3, env = environment(), folds = folds)

## ----print-est-1--------------------------------------------------------------
est_1
print(est_1)

## ----load-heart-data----------------------------------------------------------
## read in the data from the Elements website
library("RCurl")
heart_data <- read.csv(text = getURL("http://web.stanford.edu/~hastie/ElemStatLearn/datasets/SAheart.data"), header = TRUE, stringsAsFactors = FALSE)
## minor data cleaning
heart <- heart_data[, 2:dim(heart_data)[2]]
heart$famhist <- ifelse(heart$famhist == "Present", 1, 0)
## folds
heart_folds <- sample(rep(seq_len(2), length = dim(heart)[1]))

## ----est-heart-regressions-lm-------------------------------------------------
## estimate the full conditional mean using linear regression
full_mod <- lm(chd ~ ., data = subset(heart, heart_folds == 1))
full_fit <- predict(full_mod)

## estimate the reduced conditional means for each of the individual variables
X <- as.matrix(heart[, -dim(heart)[2]])[heart_folds == 2, ] # remove the outcome for the predictor matrix
red_mod_sbp <- lm(full_fit ~ X[,-1])
red_fit_sbp <- predict(red_mod_sbp)
red_mod_tob <- lm(full_fit ~ X[,-2])
red_fit_tob <- predict(red_mod_tob)
red_mod_ldl <- lm(full_fit ~ X[,-3])
red_fit_ldl <- predict(red_mod_ldl)
red_mod_adi <- lm(full_fit ~ X[,-4])
red_fit_adi <- predict(red_mod_adi)
red_mod_fam <- lm(full_fit ~ X[,-5])
red_fit_fam <- predict(red_mod_fam)
red_mod_tpa <- lm(full_fit ~ X[, -6])
red_fit_tpa <- predict(red_mod_tpa)
red_mod_obe <- lm(full_fit ~ X[,-7])
red_fit_obe <- predict(red_mod_obe)
red_mod_alc <- lm(full_fit ~ X[, -8])
red_fit_alc <- predict(red_mod_alc)
red_mod_age <- lm(full_fit ~ X[,-9])
red_fit_age <- predict(red_mod_age)

## load the library
library("vimp")
library("dplyr")

## plug these into vim
lm_vim_sbp <- vim(Y = heart$chd, f1 = full_fit, f2 = red_fit_sbp, indx = 1, run_regression = FALSE, type = "r_squared", folds = heart_folds)
lm_vim_tob <- vim(Y = heart$chd, f1 = full_fit, f2 = red_fit_tob, indx = 2, run_regression = FALSE, type = "r_squared", folds = heart_folds)
lm_vim_ldl <- vim(Y = heart$chd, f1 = full_fit, f2 = red_fit_ldl, indx = 3, run_regression = FALSE, type = "r_squared", folds = heart_folds)
lm_vim_adi <- vim(Y = heart$chd, f1 = full_fit, f2 = red_fit_adi, indx = 4, run_regression = FALSE, type = "r_squared", folds = heart_folds)
lm_vim_fam <- vim(Y = heart$chd, f1 = full_fit, f2 = red_fit_fam, indx = 5, run_regression = FALSE, type = "r_squared", folds = heart_folds)
lm_vim_tpa <- vim(Y = heart$chd, f1 = full_fit, f2 = red_fit_tpa, indx = 6, run_regression = FALSE, type = "r_squared", folds = heart_folds)
lm_vim_obe <- vim(Y = heart$chd, f1 = full_fit, f2 = red_fit_obe, indx = 7, run_regression = FALSE, type = "r_squared", folds = heart_folds)
lm_vim_alc <- vim(Y = heart$chd, f1 = full_fit, f2 = red_fit_alc, indx = 8, run_regression = FALSE, type = "r_squared", folds = heart_folds)
lm_vim_age <- vim(Y = heart$chd, f1 = full_fit, f2 = red_fit_age, indx = 9, run_regression = FALSE, type = "r_squared", folds = heart_folds)

## make a table with the estimates using the merge_vim() function
lm_mat <- merge_vim(lm_vim_sbp, lm_vim_tob, lm_vim_ldl, lm_vim_adi,
                lm_vim_fam, lm_vim_tpa, lm_vim_obe, lm_vim_alc, lm_vim_age)
## print out the matrix
lm_mat

## ----full-learner-lib---------------------------------------------------------
## load the library
library(SuperLearner)

## create a function for boosted stumps
SL.gbm.1 <- function(..., interaction.depth = 1) SL.gbm(..., interaction.depth = interaction.depth)

## create GAMs with different degrees of freedom
SL.gam.3 <- function(..., deg.gam = 3) SL.gam(..., deg.gam = deg.gam)
SL.gam.4 <- function(..., deg.gam = 4) SL.gam(..., deg.gam = deg.gam)
SL.gam.5 <- function(..., deg.gam = 5) SL.gam(..., deg.gam = deg.gam)

## add more levels of alpha for glmnet
create.SL.glmnet <- function(alpha = c(0.25, 0.5, 0.75)) {
  for (mm in seq(length(alpha))) {
    eval(parse(file = "", text = paste('SL.glmnet.', alpha[mm], '<- function(..., alpha = ', alpha[mm], ') SL.glmnet(..., alpha = alpha)', sep = '')), envir = .GlobalEnv)
  }
  invisible(TRUE)
}
create.SL.glmnet()

## add tuning parameters for randomForest
create.SL.randomForest <- function(tune = list(mtry = c(1, 5, 7), nodesize = c(1, 5, 10))) {
  tuneGrid <- expand.grid(tune, stringsAsFactors = FALSE)
  for (mm in seq(nrow(tuneGrid))) {
    eval(parse(file = "", text = paste("SL.randomForest.", mm, "<- function(..., mtry = ", tuneGrid[mm, 1], ", nodesize = ", tuneGrid[mm, 2], ") SL.randomForest(..., mtry = mtry, nodesize = nodesize)", sep = "")), envir = .GlobalEnv)
  }
  invisible(TRUE)
}
create.SL.randomForest()

## create the library
learners <- c("SL.glmnet", "SL.glmnet.0.25", "SL.glmnet.0.5", "SL.glmnet.0.75",
              "SL.randomForest", "SL.randomForest.1", "SL.randomForest.2", "SL.randomForest.3",
              "SL.randomForest.4", "SL.randomForest.5", "SL.randomForest.6", "SL.randomForest.7",
              "SL.randomForest.8", "SL.randomForest.9",
              "SL.gbm.1")

## ----vimp-with-sl-1, eval = FALSE---------------------------------------------
#  ## load the library
#  library("vimp")
#  
#  ## now estimate variable importance
#  vimp_rsquared(Y = heart$chd, X = X,
#      indx = 5, run_regression = TRUE, SL.library = learners, V = 5)

## ----vimp-with-sl-fam, message = FALSE----------------------------------------
## load the library
library("vimp")

## new learners library, with only one learner for illustration only
SL.xgboost1 <- function(..., max_depth = 1, ntree = 500, shrinkage = 0.1){
  SL.xgboost(..., max_depth = max_depth, ntree = ntree, shrinkage = shrinkage)
}
learners.2 <- c("SL.xgboost1")

## now estimate variable importance
fam_vim <- vim(Y = heart$chd, X = heart[, -dim(heart)[2]], indx = 5, run_regression = TRUE, SL.library = learners.2, na.rm = TRUE, env = environment(), type = "r_squared", folds = heart_folds)

## ----print-fam-vim------------------------------------------------------------
fam_vim

## ----look-at-fam-ests---------------------------------------------------------
head(fam_vim$full_fit)
head(fam_vim$red_fit)

## ----typea-vim, message = FALSE-----------------------------------------------
## specify that full_fit doesn't change
full_fit <- fam_vim$full_fit

## estimate variable importance for the average number of rooms
reduced_fit <- SuperLearner::SuperLearner(Y = full_fit, X = heart[heart_folds == 2, -c(6, dim(heart)[2]), drop = FALSE], SL.library = learners.2)
red_fit <- predict(reduced_fit)$pred
tpa_vim <- vim(Y = heart$chd, f1 = full_fit, f2 = red_fit, indx = 6, run_regression = FALSE, type = "r_squared", folds = heart_folds)

tpa_vim

## ----alc-vim------------------------------------------------------------------
## set up the data, removing the columns for alcohol use and chd
x <- heart[, -c(8, dim(heart)[2])]

## fit an xgboost model using SuperLearner
reduced_mod <- SuperLearner(Y = full_fit, X = x[heart_folds == 2, ], SL.library = learners.2)
reduced_fit <- predict(reduced_mod)$pred
## this takes 2 seconds

## estimate variable importance
alc_vim <- vim(Y = heart$chd, f1 = full_fit, f2 = reduced_fit, indx = 8, run_regression = FALSE, type = "r_squared", folds = heart_folds)

## ----heart-sl-----------------------------------------------------------------
reduced_sbp <- predict(SuperLearner(Y = full_fit, X = heart[heart_folds == 2, -c(1, dim(heart)[2])], SL.library = learners.2))$pred
sbp_vim <- vim(Y = heart$chd, f1 = full_fit, f2 = reduced_sbp,
                indx = 1, run_regression = FALSE, type = "r_squared", folds = heart_folds)

reduced_tob <- predict(SuperLearner(Y = full_fit, X = heart[heart_folds == 2, -c(2, dim(heart)[2])], SL.library = learners.2))$pred
tob_vim <- vim(Y = heart$chd, f1 = full_fit, f2 = reduced_tob,
                indx = 2, run_regression = FALSE, type = "r_squared", folds = heart_folds)

reduced_ldl <- predict(SuperLearner(Y = full_fit, X = heart[heart_folds == 2, -c(3, dim(heart)[2])], SL.library = learners.2))$pred
ldl_vim <- vim(Y = heart$chd, f1 = full_fit, f2 = reduced_ldl,
                indx = 3, run_regression = FALSE, type = "r_squared", folds = heart_folds)

reduced_adi <- predict(SuperLearner(Y = full_fit, X = heart[heart_folds == 2, -c(4, dim(heart)[2])], SL.library = learners.2))$pred
adi_vim <- vim(Y = heart$chd, f1 = full_fit, f2 = reduced_adi,
                indx = 4, run_regression = FALSE, type = "r_squared", folds = heart_folds)

reduced_obe <- predict(SuperLearner(Y = full_fit, X = heart[heart_folds == 2, -c(7, dim(heart)[2])], SL.library = learners.2))$pred
obe_vim <- vim(Y = heart$chd, f1 = full_fit, f2 = reduced_obe,
                indx = 7, run_regression = FALSE, type = "r_squared", folds = heart_folds)

reduced_age <- predict(SuperLearner(Y = full_fit, X = heart[heart_folds == 2, -c(9, dim(heart)[2])], SL.library = learners.2))$pred
age_vim <- vim(Y = heart$chd, f1 = full_fit, f2 = reduced_age,
                indx = 9, run_regression = FALSE, type = "r_squared", folds = heart_folds)

## ----heart-vim, fig.width = 8.5, fig.height = 8-------------------------------
library("dplyr")
library("tibble")
library("ggplot2")
library("cowplot")
theme_set(theme_cowplot())
library("forcats")
## combine the objects together
ests <- merge_vim(sbp_vim, tob_vim, ldl_vim, adi_vim,
                fam_vim, tpa_vim, obe_vim, alc_vim, age_vim)

get_nm <- function(s) {
  if (s == 1) {
    return("Sys. blood press.")
  } else if (s == 2) {
    return("Tobacco consump.")
  } else if (s == 3) {
    return("LDL cholesterol")
  } else if (s == 4) {
    return("Adiposity")
  } else if (s == 5) {
    return("Family history")
  } else if (s == 6) {
    return("Type A behavior")
  } else if (s == 7) {
    return("Obesity")
  } else if (s == 8) {
    return("Alcohol consump.")
  } else if (s == 9) {
    return("Age")
  }
}
get_nms <- function(ests) {
  return(apply(matrix(as.numeric(ests$s)), 1, get_nm))
}

## plot
ests$mat %>%
  arrange(desc(est)) %>%
  mutate(ord_group = forcats::fct_reorder(get_nms(ests$mat), est)) %>%
  ggplot(aes(x = est, y = ord_group)) +
  geom_point() +
  geom_errorbarh(aes(xmin = cil, xmax = ciu)) +
  xlab(expression(paste("Variable importance estimates: ", R^2, sep = ""))) +
  ylab("") +
  ggtitle("Variable importance estimates for individual features in the South African heart disease study data")

## ----heart-group-vim, fig.width = 8.5, fig.height = 8-------------------------
## get the estimates
reduced_behav <- predict(SuperLearner(Y = heart$chd[heart_folds == 2], X = heart[heart_folds == 2, -c(2, 6, 8, dim(heart)[2])], SL.library = learners.2))$pred
behav_vim <- vim(Y = heart$chd, f1 = full_fit, f2 = reduced_behav, indx = c(2, 6, 8), run_regression = FALSE, type = "r_squared", folds = heart_folds)

reduced_bios <- predict(SuperLearner(Y = heart$chd[heart_folds == 2], X = heart[heart_folds == 2, -c(1, 3, 4, 5, 7, 9, dim(heart)[2])], SL.library = learners.2))$pred
bios_vim <- vim(Y = heart$chd, f1 = full_fit, f2 = reduced_bios, indx = c(1, 3, 4, 5, 7, 9), run_regression = FALSE, type = "r_squared", folds = heart_folds)

## combine and plot
groups <- merge_vim(behav_vim, bios_vim)
nms.2 <- c("Behavioral features", "Biological features")
nms_grp <- function(s) {
  if (s == "1,3,4,5,7,9") {
    return("Biological features")
  } else {
    return("Behavioral features")
  }
}
get_nms_grp <- function(ests) {
  return(unlist(lapply(as.list(ests$s), function(x) nms_grp(x))))
}
groups$mat %>%
  arrange(desc(est)) %>%
  mutate(ord_group = forcats::fct_reorder(get_nms_grp(groups$mat), est)) %>%
  ggplot(aes(x = est, y = ord_group)) +
  geom_point() +
  geom_errorbarh(aes(xmin = cil, xmax = ciu)) +
  xlab(expression(paste("Variable importance estimates: ", R^2, sep = ""))) +
  ylab("") +
  ggtitle("Variable importance estimates for groups of features in the South African heart disease study data")

