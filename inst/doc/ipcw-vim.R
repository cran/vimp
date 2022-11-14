## ----setup, echo = FALSE, include = FALSE-------------------------------------
library(knitr)
opts_knit$set(cache = FALSE, verbose = TRUE, global.par = TRUE)

## ----more-setup, echo = FALSE-------------------------------------------------
par(mar = c(5, 12, 4, 2) + 0.1)

## ----gen-data-missing-y-------------------------------------------------------
set.seed(1234)
p <- 2
n <- 100
x <- replicate(p, stats::rnorm(n, 0, 1))
# apply the function to the x's
y <- 1 + 0.5 * x[, 1] + 0.75 * x[, 2] + stats::rnorm(n, 0, 1)
# indicator of observing Y
logit_g_x <- .01 * x[, 1] + .05 * x[, 2] - 2.5
g_x <- exp(logit_g_x) / (1 + exp(logit_g_x))
C <- rbinom(n, size = 1, prob = g_x)
obs_y <- y
obs_y[C == 0] <- NA
x_df <- as.data.frame(x)
full_df <- data.frame(Y = obs_y, x_df, C = C)

## ----missing-y-vim------------------------------------------------------------
library("vimp")
library("SuperLearner")
# estimate the probability of missing outcome
ipc_weights <- 1 / predict(glm(C ~ V1 + V2, family = "binomial", data = full_df),
                           type = "response")

# set up the SL
learners <- c("SL.glm", "SL.mean")
V <- 2

# estimate vim for X2
set.seed(1234)
est <- vim(Y = obs_y, X = x_df, indx = 2, type = "r_squared", run_regression = TRUE,
           SL.library = learners, alpha = 0.05, delta = 0, C = C, Z = c("Y", "1"),
           ipc_weights = ipc_weights, cvControl = list(V = V))

## ----generate-data------------------------------------------------------------
set.seed(4747)
p <- 2
n <- 100
x <- replicate(p, stats::rnorm(n, 0, 1))
# apply the function to the x's
y <- 1 + 0.5 * x[, 1] + 0.75 * x[, 2] + stats::rnorm(n, 0, 1)
# make this a two-phase study, assume that X2 is only measured on
# subjects in the second phase; note C = 1 is inclusion
C <- rbinom(n, size = 1, prob = exp(y + 0.1 * x[, 1]) / (1 + exp(y + 0.1 * x[, 1])))
tmp_x <- x
tmp_x[C == 0, 2] <- NA
x <- tmp_x
x_df <- as.data.frame(x)
full_df <- data.frame(Y = y, x_df, C = C)

## ----ipw-vim------------------------------------------------------------------
library("vimp")
library("SuperLearner")
# estimate the probability of being included only in the first phase sample
ipc_weights <- 1 / predict(glm(C ~ y + V1, family = "binomial", data = full_df),
                           type = "response")

# set up the SL
learners <- c("SL.glm")
V <- 2

# estimate vim for X2
set.seed(1234)
est <- vim(Y = y, X = x_df, indx = 2, type = "r_squared", run_regression = TRUE,
           SL.library = learners, alpha = 0.05, delta = 0, C = C, Z = c("Y", "1"),
           ipc_weights = ipc_weights, cvControl = list(V = V), method = "method.CC_LS")

