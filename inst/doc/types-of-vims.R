## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library("vimp")
library("SuperLearner")

## ----load-heart-data----------------------------------------------------------
## read in the data from the Elements website
library("RCurl")
heart_data <- read.csv(text = getURL("http://web.stanford.edu/~hastie/ElemStatLearn/datasets/SAheart.data"), header = TRUE, stringsAsFactors = FALSE)
## minor data cleaning
heart <- heart_data[, 2:dim(heart_data)[2]]
heart$famhist <- ifelse(heart$famhist == "Present", 1, 0)
x <- heart[, -ncol(heart)]
# a simple library, to speed up the vignette;
# in general, we recommend fitting a flexible library
learners.2 <- c("SL.glm")
set.seed(12345)

## ----est-famhist-cond---------------------------------------------------------
# note the use of a small V and a small number of SL folds, for illustration only
V <- 2
sl_cvcontrol <- list(V = 2)
fam_vim_cond <- vimp_rsquared(Y = heart$chd, X = x, indx = 5, SL.library = learners.2, na.rm = TRUE, V = V, cvControl = sl_cvcontrol)

## ----est-famhist-marg---------------------------------------------------------
# note the use of a small V and a small number of SL folds, for illustration only
fam_vim_marg <- vimp_rsquared(Y = heart$chd, X = x[, 5, drop = FALSE], indx = 1, SL.library = learners.2, na.rm = TRUE, V = V, cvControl = sl_cvcontrol)

## ----est-famhist-spvim--------------------------------------------------------
all_vim_spvim <- sp_vim(Y = heart$chd, X = x, type = "r_squared", SL.library = learners.2, na.rm = TRUE, V = V, cvControl = sl_cvcontrol, env = environment())

## ----show-ests----------------------------------------------------------------
fam_vim_cond
fam_vim_marg
# note: need to look at row for s = 5
all_vim_spvim

