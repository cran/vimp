## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
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
learners <- "SL.glm"

## ----est-subtype-01AE-cond, warning = FALSE-----------------------------------
# note the use of a small V and a small number of SL folds, for illustration only
set.seed(1234)
V <- 2
sl_cvcontrol <- list(V = 2)
subtype_01_AE_cond <- vimp_auc(Y = y, X = X, indx = 5, SL.library = learners, na.rm = TRUE, V = V, cvControl = sl_cvcontrol)

## ----est-subtype-01AE-marg, warning = FALSE-----------------------------------
# note the use of a small V and a small number of SL folds, for illustration only
set.seed(5678)
subtype_01_AE_marg <- vimp_auc(Y = y, X = X[, 5, drop = FALSE], indx = 1, SL.library = learners, na.rm = TRUE, V = V, cvControl = sl_cvcontrol)

## ----est-famhist-spvim, warning = FALSE---------------------------------------
set.seed(91011)
all_vim_spvim <- sp_vim(Y = y, X = X, type = "auc", SL.library = learners, na.rm = TRUE, V = V, cvControl = sl_cvcontrol, env = environment())

## ----show-ests----------------------------------------------------------------
subtype_01_AE_cond
subtype_01_AE_marg
# note: need to look at row for s = 5
all_vim_spvim

