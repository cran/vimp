% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vimp_deviance.R
\name{vimp_deviance}
\alias{vimp_deviance}
\title{Nonparametric Intrinsic Variable Importance Estimates: Deviance}
\usage{
vimp_deviance(
  Y = NULL,
  X = NULL,
  cross_fitted_f1 = NULL,
  cross_fitted_f2 = NULL,
  f1 = NULL,
  f2 = NULL,
  indx = 1,
  V = 10,
  run_regression = TRUE,
  SL.library = c("SL.glmnet", "SL.xgboost", "SL.mean"),
  alpha = 0.05,
  delta = 0,
  na.rm = FALSE,
  final_point_estimate = "split",
  cross_fitting_folds = NULL,
  sample_splitting_folds = NULL,
  stratified = TRUE,
  C = rep(1, length(Y)),
  Z = NULL,
  ipc_weights = rep(1, length(Y)),
  scale = "logit",
  ipc_est_type = "aipw",
  scale_est = TRUE,
  cross_fitted_se = TRUE,
  ...
)
}
\arguments{
\item{Y}{the outcome.}

\item{X}{the covariates. If \code{type = "average_value"}, then the exposure
variable should be part of \code{X}, with its name provided in \code{exposure_name}.}

\item{cross_fitted_f1}{the predicted values on validation data from a
flexible estimation technique regressing Y on X in the training data. Provided as
either (a) a vector, where each element is
the predicted value when that observation is part of the validation fold;
or (b) a list of length V, where each element in the list is a set of predictions on the
corresponding validation data fold.
If sample-splitting is requested, then these must be estimated specially; see Details. However,
the resulting vector should be the same length as \code{Y}; if using a list, then the summed
length of each element across the list should be the same length as \code{Y} (i.e.,
each observation is included in the predictions).}

\item{cross_fitted_f2}{the predicted values on validation data from a
flexible estimation technique regressing either (a) the fitted values in
\code{cross_fitted_f1}, or (b) Y, on X withholding the columns in \code{indx}.
Provided as either (a) a vector, where each element is
the predicted value when that observation is part of the validation fold;
or (b) a list of length V, where each element in the list is a set of predictions on the
corresponding validation data fold.
If sample-splitting is requested, then these must be estimated specially; see Details. However,
the resulting vector should be the same length as \code{Y}; if using a list, then the summed
length of each element across the list should be the same length as \code{Y} (i.e.,
each observation is included in the predictions).}

\item{f1}{the fitted values from a flexible estimation technique
regressing Y on X. If sample-splitting is requested, then these must be
estimated specially; see Details. If \code{cross_fitted_se = TRUE},
then this argument is not used.}

\item{f2}{the fitted values from a flexible estimation technique
regressing either (a) \code{f1} or (b) Y on X withholding the columns in
\code{indx}. If sample-splitting is requested, then these must be
estimated specially; see Details. If \code{cross_fitted_se = TRUE},
then this argument is not used.}

\item{indx}{the indices of the covariate(s) to calculate variable
importance for; defaults to 1.}

\item{V}{the number of folds for cross-fitting, defaults to 5. If
\code{sample_splitting = TRUE}, then a special type of \code{V}-fold cross-fitting
is done. See Details for a more detailed explanation.}

\item{run_regression}{if outcome Y and covariates X are passed to
\code{vimp_accuracy}, and \code{run_regression} is \code{TRUE},
then Super Learner will be used; otherwise, variable importance
will be computed using the inputted fitted values.}

\item{SL.library}{a character vector of learners to pass to
\code{SuperLearner}, if \code{f1} and \code{f2} are Y and X,
respectively. Defaults to \code{SL.glmnet}, \code{SL.xgboost},
and \code{SL.mean}.}

\item{alpha}{the level to compute the confidence interval at.
Defaults to 0.05, corresponding to a 95\% confidence interval.}

\item{delta}{the value of the \eqn{\delta}-null (i.e., testing if
importance < \eqn{\delta}); defaults to 0.}

\item{na.rm}{should we remove NAs in the outcome and fitted values
in computation? (defaults to \code{FALSE})}

\item{final_point_estimate}{if sample splitting is used, should the final point estimates
be based on only the sample-split folds used for inference (\code{"split"}, the default),
or should they instead be based on the full dataset (\code{"full"}) or the average
across the point estimates from each sample split (\code{"average"})? All three
options result in valid point estimates -- sample-splitting is only required for valid inference.}

\item{cross_fitting_folds}{the folds for cross-fitting. Only used if
\code{run_regression = FALSE}.}

\item{sample_splitting_folds}{the folds used for sample-splitting;
these identify the observations that should be used to evaluate
predictiveness based on the full and reduced sets of covariates, respectively.
Only used if \code{run_regression = FALSE}.}

\item{stratified}{if run_regression = TRUE, then should the generated
folds be stratified based on the outcome (helps to ensure class balance
across cross-validation folds)}

\item{C}{the indicator of coarsening (1 denotes observed, 0 denotes
unobserved).}

\item{Z}{either (i) NULL (the default, in which case the argument
\code{C} above must be all ones), or (ii) a character vector
specifying the variable(s) among Y and X that are thought to play a
role in the coarsening mechanism. To specify the outcome, use \code{"Y"}; to
specify covariates, use a character number corresponding to the desired
position in X (e.g., \code{"1"}).}

\item{ipc_weights}{weights for the computed influence curve (i.e.,
inverse probability weights for coarsened-at-random settings).
Assumed to be already inverted (i.e., ipc_weights = 1 / [estimated
probability weights]).}

\item{scale}{should CIs be computed on original ("identity") or
another scale? (options are "log" and "logit")}

\item{ipc_est_type}{the type of procedure used for coarsened-at-random
settings; options are "ipw" (for inverse probability weighting) or
"aipw" (for augmented inverse probability weighting).
Only used if \code{C} is not all equal to 1.}

\item{scale_est}{should the point estimate be scaled to be greater than or equal to 0?
Defaults to \code{TRUE}.}

\item{cross_fitted_se}{should we use cross-fitting to estimate the standard
errors (\code{TRUE}, the default) or not (\code{FALSE})?}

\item{...}{other arguments to the estimation tool, see "See also".}
}
\value{
An object of classes \code{vim} and \code{vim_deviance}.
  See Details for more information.
}
\description{
Compute estimates of and confidence intervals for nonparametric
deviance-based intrinsic variable importance. This is a wrapper function for
\code{cv_vim}, with \code{type = "deviance"}.
}
\details{
We define the population variable importance measure (VIM) for the
group of features (or single feature) \eqn{s} with respect to the
predictiveness measure \eqn{V} by
\deqn{\psi_{0,s} := V(f_0, P_0) - V(f_{0,s}, P_0),} where \eqn{f_0} is
the population predictiveness maximizing function, \eqn{f_{0,s}} is the
population predictiveness maximizing function that is only allowed to access
the features with index not in \eqn{s}, and \eqn{P_0} is the true
data-generating distribution.

Cross-fitted VIM estimates are computed differently if sample-splitting
is requested versus if it is not. We recommend using sample-splitting
in most cases, since only in this case will inferences be valid if
the variable(s) of interest have truly zero population importance.
The purpose of cross-fitting is to estimate \eqn{f_0} and \eqn{f_{0,s}}
on independent data from estimating \eqn{P_0}; this can result in improved
performance, especially when using flexible learning algorithms. The purpose
of sample-splitting is to estimate \eqn{f_0} and \eqn{f_{0,s}} on independent
data; this allows valid inference under the null hypothesis of zero importance.

Without sample-splitting, cross-fitted VIM estimates are obtained by first
splitting the data into \eqn{K} folds; then using each fold in turn as a
hold-out set, constructing estimators \eqn{f_{n,k}} and \eqn{f_{n,k,s}} of
\eqn{f_0} and \eqn{f_{0,s}}, respectively on the training data and estimator
\eqn{P_{n,k}} of \eqn{P_0} using the test data; and finally, computing
\deqn{\psi_{n,s} := K^{(-1)}\sum_{k=1}^K \{V(f_{n,k},P_{n,k}) - V(f_{n,k,s}, P_{n,k})\}.}

With sample-splitting, cross-fitted VIM estimates are obtained by first
splitting the data into \eqn{2K} folds. These folds are further divided
into 2 groups of folds. Then, for each fold \eqn{k} in the first group,
estimator \eqn{f_{n,k}} of \eqn{f_0} is constructed using all data besides
the kth fold in the group (i.e., \eqn{(2K - 1)/(2K)} of the data) and
estimator \eqn{P_{n,k}} of \eqn{P_0} is constructed using the held-out data
(i.e., \eqn{1/2K} of the data); then, computing
\deqn{v_{n,k} = V(f_{n,k},P_{n,k}).}
Similarly, for each fold \eqn{k} in the second group,
estimator \eqn{f_{n,k,s}} of \eqn{f_{0,s}} is constructed using all data
besides the kth fold in the group (i.e., \eqn{(2K - 1)/(2K)} of the data)
and estimator \eqn{P_{n,k}} of \eqn{P_0} is constructed using the held-out
data (i.e., \eqn{1/2K} of the data); then, computing
\deqn{v_{n,k,s} = V(f_{n,k,s},P_{n,k}).}
Finally,
\deqn{\psi_{n,s} := K^{(-1)}\sum_{k=1}^K \{v_{n,k} - v_{n,k,s}\}.}

See the paper by Williamson, Gilbert, Simon, and Carone for more
details on the mathematics behind the \code{cv_vim} function, and the
validity of the confidence intervals.

In the interest of transparency, we return most of the calculations
within the \code{vim} object. This results in a list including:
\describe{
 \item{s}{the column(s) to calculate variable importance for}
 \item{SL.library}{the library of learners passed to \code{SuperLearner}}
 \item{full_fit}{the fitted values of the chosen method fit to the full data (a list, for train and test data)}
 \item{red_fit}{the fitted values of the chosen method fit to the reduced data (a list, for train and test data)}
 \item{est}{the estimated variable importance}
 \item{naive}{the naive estimator of variable importance}
 \item{eif}{the estimated efficient influence function}
 \item{eif_full}{the estimated efficient influence function for the full regression}
 \item{eif_reduced}{the estimated efficient influence function for the reduced regression}
 \item{se}{the standard error for the estimated variable importance}
 \item{ci}{the \eqn{(1-\alpha) \times 100}\% confidence interval for the variable importance estimate}
 \item{test}{a decision to either reject (TRUE) or not reject (FALSE) the null hypothesis, based on a conservative test}
 \item{p_value}{a p-value based on the same test as \code{test}}
 \item{full_mod}{the object returned by the estimation procedure for the full data regression (if applicable)}
 \item{red_mod}{the object returned by the estimation procedure for the reduced data regression (if applicable)}
 \item{alpha}{the level, for confidence interval calculation}
 \item{sample_splitting_folds}{the folds used for hypothesis testing}
 \item{cross_fitting_folds}{the folds used for cross-fitting}
 \item{y}{the outcome}
 \item{ipc_weights}{the weights}
 \item{cluster_id}{the cluster IDs}
 \item{mat}{a tibble with the estimate, SE, CI, hypothesis testing decision, and p-value}
}
}
\examples{
# generate the data
# generate X
p <- 2
n <- 100
x <- data.frame(replicate(p, stats::runif(n, -1, 1)))

# apply the function to the x's
f <- function(x) 0.5 + 0.3*x[1] + 0.2*x[2]
smooth <- apply(x, 1, function(z) f(z))

# generate Y ~ Normal (smooth, 1)
y <- matrix(stats::rbinom(n, size = 1, prob = smooth))

# set up a library for SuperLearner; note simple library for speed
library("SuperLearner")
learners <- c("SL.glm", "SL.mean")

# estimate (with a small number of folds, for illustration only)
est <- vimp_deviance(y, x, indx = 2,
           alpha = 0.05, run_regression = TRUE,
           SL.library = learners, V = 2, cvControl = list(V = 2))

}
\seealso{
\code{\link[SuperLearner]{SuperLearner}} for specific usage of the \code{SuperLearner} function and package.
}
