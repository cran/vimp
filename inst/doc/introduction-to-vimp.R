## ----setup, echo = FALSE, include = FALSE-------------------------------------
library(knitr)
opts_knit$set(cache = FALSE, verbose = TRUE, global.par = TRUE)

## ----more-setup, echo = FALSE-------------------------------------------------
par(mar = c(5, 12, 4, 2) + 0.1)

## ----install-vimp, eval = FALSE-----------------------------------------------
# install.packages("vimp")

## ----devtools-install-vimp, eval = FALSE--------------------------------------
# # only run if you don't have devtools
# # previously installed
# # install.packages("devtools")
# devtools::install_github("bdwilliamson/vimp")

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

## ----est-1, warning = FALSE---------------------------------------------------
est_1 <- vimp_rsquared(Y = y, X = x, indx = 1, run_regression = TRUE,
                       SL.library = c("SL.ranger", "SL.mean"), V = 2,
                       env = environment())

## ----print-est-1--------------------------------------------------------------
est_1
print(est_1)

## ----load-vrc01-data----------------------------------------------------------
# read in the data
data("vrc01")

## ----subset-data--------------------------------------------------------------
library("dplyr")
library("tidyselect")
# retain only the columns of interest for this analysis
y <- vrc01$ic50.censored
X <- vrc01 %>%
  select(starts_with("geog"), starts_with("subtype"), starts_with("length"))

## ----est-regressions-lm, warning = FALSE--------------------------------------
geog_indx <- max(which(grepl("geog", names(X))))
set.seed(1234)
for (i in seq_len(ncol(X) - geog_indx)) {
  # note that we're using a small number of cross-fitting folds for speed
  lm_vim <- vim(Y = y, X = X, indx = geog_indx + i, run_regression = TRUE, SL.library = "SL.glm", type = "r_squared", cvControl = list(V = 2), scale = "logit", family = binomial())
  if (i == 1) {
    lm_mat <- lm_vim
  } else {
    lm_mat <- merge_vim(lm_mat, lm_vim)
  }
}
# print out the importance
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
# vimp_rsquared(Y = y, X = X,
#     indx = 5, run_regression = TRUE, SL.library = learners, V = 5, family = binomial())

## ----vimp-with-sl-fam, message = FALSE, warning = FALSE-----------------------
# small learners library
learners.2 <- c("SL.ranger")
# small number of cross-fitting folds
V <- 2
# small number of CV folds for Super Learner
sl_cvcontrol <- list(V = 2)

# now estimate variable importance
set.seed(5678)
start_time <- Sys.time()
subtype_01_AE_vim <- vimp_rsquared(Y = y, X = X, indx = 5, SL.library = learners.2, na.rm = TRUE, env = environment(), V = V, cvControl = sl_cvcontrol, family = binomial())
end_time <- Sys.time()

## ----print-vim----------------------------------------------------------------
subtype_01_AE_vim

## ----look-at-ests-------------------------------------------------------------
head(subtype_01_AE_vim$full_fit[[1]])
head(subtype_01_AE_vim$red_fit[[1]])

## ----vrc01-sl, warning = FALSE------------------------------------------------
ests <- subtype_01_AE_vim
set.seed(1234)
for (i in seq_len(ncol(X) - geog_indx - 1)) {
  # note that we're using a small number of cross-fitting folds for speed
  this_vim <- vimp_rsquared(Y = y, X = X, indx = geog_indx + i + 1, run_regression = TRUE, SL.library = learners.2, V = V, cvControl = sl_cvcontrol, family = binomial())
  ests <- merge_vim(ests, this_vim)
}

## ----vrc01-vim, fig.width = 8.5, fig.height = 8, message = FALSE--------------
library("ggplot2")
library("cowplot")
theme_set(theme_cowplot())
all_vars <- c(paste0("Subtype is ", c("01_AE", "02_AG", "07_BC", "A1", "A1C", "A1D",
                                      "B", "C", "D", "O", "Other")),
              paste0("Length of ", c("Env", "gp120", "V5", "V5 outliers", "Loop E",
                                     "Loop E outliers")))

est_plot_tib <- ests$mat %>%
  mutate(
    var_fct = rev(factor(s, levels = ests$mat$s,
                     labels = all_vars[as.numeric(ests$mat$s) - geog_indx],
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
  labs(subtitle = "in the VRC01 data (considering only geographic confounders, subtype, and viral geometry)")

## ----vrc01-group-vim, fig.width = 8.5, fig.height = 8-------------------------
# get the estimates
set.seed(91011)
subtype_vim <- vimp_rsquared(Y = y, X = X, indx = 5:15, SL.library = learners.2, na.rm = TRUE, env = environment(), V = V, cvControl = sl_cvcontrol, family = binomial())
geometry_vim <- vimp_rsquared(Y = y, X = X, indx = 16:21, SL.library = learners.2, na.rm = TRUE, env = environment(), V = V, cvControl = sl_cvcontrol, family = binomial())

# combine and plot
groups <- merge_vim(subtype_vim, geometry_vim)
all_grp_nms <- c("Viral subtype", "Viral geometry")

grp_plot_tib <- groups$mat %>%
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

