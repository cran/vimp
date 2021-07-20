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

## ----load-vimp, message = FALSE-----------------------------------------------
library("vimp")

## ----gen-data-----------------------------------------------------------------
# -------------------------------------------------------------
# problem setup
# -------------------------------------------------------------
# set up the data
set.seed(5678910)
n <- 1000
p <- 2
s <- 1 # desire importance for X_1
x <- data.frame(replicate(p, runif(n, -1, 1)))
y <- (x[,1])^2*(x[,1]+7/5) + (25/9)*(x[,2])^2 + rnorm(n, 0, 1)
# set up folds for hypothesis testing
folds <- sample(rep(seq_len(2), length = length(y)))

## ----learner-lib-small, message = FALSE---------------------------------------
library("SuperLearner")
# load specific algorithms
library("ranger")

## ----est-1--------------------------------------------------------------------
est_1 <- vimp_rsquared(Y = y, X = x, indx = 1, run_regression = TRUE, 
                       SL.library = c("SL.ranger", "SL.mean"), V = 2,
                       env = environment())

## ----print-est-1--------------------------------------------------------------
est_1
print(est_1)

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
X <- heart[, -ncol(heart)]
lm_vim_sbp <- vim(Y = heart$chd, X = X, indx = 1, run_regression = TRUE, SL.library = "SL.lm", type = "r_squared")
lm_vim_tob <- vim(Y = heart$chd, X = X, indx = 2, run_regression = TRUE, SL.library = "SL.lm", type = "r_squared")
lm_vim_ldl <- vim(Y = heart$chd, X = X, indx = 3, run_regression = TRUE, SL.library = "SL.lm", type = "r_squared")
lm_vim_adi <- vim(Y = heart$chd, X = X, indx = 4, run_regression = TRUE, SL.library = "SL.lm", type = "r_squared")
lm_vim_fam <- vim(Y = heart$chd, X = X, indx = 5, run_regression = TRUE, SL.library = "SL.lm", type = "r_squared")
lm_vim_tpa <- vim(Y = heart$chd, X = X, indx = 6, run_regression = TRUE, SL.library = "SL.lm", type = "r_squared")
lm_vim_obe <- vim(Y = heart$chd, X = X, indx = 7, run_regression = TRUE, SL.library = "SL.lm", type = "r_squared")
lm_vim_alc <- vim(Y = heart$chd, X = X, indx = 8, run_regression = TRUE, SL.library = "SL.lm", type = "r_squared")
lm_vim_age <- vim(Y = heart$chd, X = X, indx = 9, run_regression = TRUE, SL.library = "SL.lm", type = "r_squared")

# make a table with the estimates using the merge_vim() function
library("dplyr")
lm_mat <- merge_vim(lm_vim_sbp, lm_vim_tob, lm_vim_ldl, lm_vim_adi,
                lm_vim_fam, lm_vim_tpa, lm_vim_obe, lm_vim_alc, lm_vim_age)
# print out the matrix
lm_mat

## ----full-learner-lib---------------------------------------------------------
# create a function for boosted stumps
SL.gbm.1 <- function(..., interaction.depth = 1) SL.gbm(..., interaction.depth = interaction.depth)

# create GAMs with different degrees of freedom
SL.gam.3 <- function(..., deg.gam = 3) SL.gam(..., deg.gam = deg.gam)
SL.gam.4 <- function(..., deg.gam = 4) SL.gam(..., deg.gam = deg.gam)
SL.gam.5 <- function(..., deg.gam = 5) SL.gam(..., deg.gam = deg.gam)

# add more levels of alpha for glmnet
create.SL.glmnet <- function(alpha = c(0.25, 0.5, 0.75)) {
  for (mm in seq(length(alpha))) {
    eval(parse(file = "", text = paste('SL.glmnet.', alpha[mm], '<- function(..., alpha = ', alpha[mm], ') SL.glmnet(..., alpha = alpha)', sep = '')), envir = .GlobalEnv)
  }
  invisible(TRUE)
}
create.SL.glmnet()

# add tuning parameters for randomForest
create.SL.randomForest <- function(tune = list(mtry = c(1, 5, 7), nodesize = c(1, 5, 10))) {
  tuneGrid <- expand.grid(tune, stringsAsFactors = FALSE)
  for (mm in seq(nrow(tuneGrid))) {
    eval(parse(file = "", text = paste("SL.randomForest.", mm, "<- function(..., mtry = ", tuneGrid[mm, 1], ", nodesize = ", tuneGrid[mm, 2], ") SL.randomForest(..., mtry = mtry, nodesize = nodesize)", sep = "")), envir = .GlobalEnv)
  }
  invisible(TRUE)
}
create.SL.randomForest()

# create the library
learners <- c("SL.glmnet", "SL.glmnet.0.25", "SL.glmnet.0.5", "SL.glmnet.0.75",
              "SL.randomForest", "SL.randomForest.1", "SL.randomForest.2", "SL.randomForest.3",
              "SL.randomForest.4", "SL.randomForest.5", "SL.randomForest.6", "SL.randomForest.7",
              "SL.randomForest.8", "SL.randomForest.9",
              "SL.gbm.1")

## ----vimp-with-sl-1, eval = FALSE---------------------------------------------
#  vimp_rsquared(Y = heart$chd, X = X,
#      indx = 5, run_regression = TRUE, SL.library = learners, V = 5)

## ----vimp-with-sl-fam, message = FALSE----------------------------------------
# small learners library
learners.2 <- c("SL.ranger")
# small number of cross-fitting folds
V <- 2
# small number of CV folds for Super Learner
sl_cvcontrol <- list(V = 2)

# now estimate variable importance
start_time <- Sys.time()
fam_vim <- vimp_rsquared(Y = heart$chd, X = X, indx = 5, SL.library = learners.2, na.rm = TRUE, env = environment(), V = V, cvControl = sl_cvcontrol)
end_time <- Sys.time()

## ----print-fam-vim------------------------------------------------------------
fam_vim

## ----look-at-fam-ests---------------------------------------------------------
head(fam_vim$full_fit)
head(fam_vim$red_fit)

## ----heart-sl-----------------------------------------------------------------
# estimate variable importance
tpa_vim <- vimp_rsquared(Y = heart$chd, X = X, indx = 6, SL.library = learners.2, na.rm = TRUE, env = environment(), V = V, cvControl = sl_cvcontrol)
alc_vim <- vimp_rsquared(Y = heart$chd, X = X, indx = 8, SL.library = learners.2, na.rm = TRUE, env = environment(), V = V, cvControl = sl_cvcontrol)
sbp_vim <- vimp_rsquared(Y = heart$chd, X = X, indx = 1, SL.library = learners.2, na.rm = TRUE, env = environment(), V = V, cvControl = sl_cvcontrol)
tob_vim <- vimp_rsquared(Y = heart$chd, X = X, indx = 2, SL.library = learners.2, na.rm = TRUE, env = environment(), V = V, cvControl = sl_cvcontrol)
ldl_vim <- vimp_rsquared(Y = heart$chd, X = X, indx = 3, SL.library = learners.2, na.rm = TRUE, env = environment(), V = V, cvControl = sl_cvcontrol)
adi_vim <- vimp_rsquared(Y = heart$chd, X = X, indx = 4, SL.library = learners.2, na.rm = TRUE, env = environment(), V = V, cvControl = sl_cvcontrol)
obe_vim <- vimp_rsquared(Y = heart$chd, X = X, indx = 7, SL.library = learners.2, na.rm = TRUE, env = environment(), V = V, cvControl = sl_cvcontrol)
age_vim <- vimp_rsquared(Y = heart$chd, X = X, indx = 9, SL.library = learners.2, na.rm = TRUE, env = environment(), V = V, cvControl = sl_cvcontrol)

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
behav_vim <- vimp_rsquared(Y = heart$chd, X = X, indx = c(2, 6, 8), SL.library = learners.2, na.rm = TRUE, env = environment(), V = V, cvControl = sl_cvcontrol)
bios_vim <- vimp_rsquared(Y = heart$chd, X = X, indx = c(1, 3, 4, 5, 7, 9), SL.library = learners.2, na.rm = TRUE, env = environment(), V = V, cvControl = sl_cvcontrol)

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

