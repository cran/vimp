% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/est_predictiveness_cv.R
\name{est_predictiveness_cv}
\alias{est_predictiveness_cv}
\title{Estimate a nonparametric predictiveness functional using cross-fitting}
\usage{
est_predictiveness_cv(
  fitted_values,
  y,
  full_y = NULL,
  folds,
  type = "r_squared",
  C = rep(1, length(y)),
  Z = NULL,
  folds_Z = folds,
  ipc_weights = rep(1, length(C)),
  ipc_fit_type = "external",
  ipc_eif_preds = rep(1, length(C)),
  ipc_est_type = "aipw",
  scale = "identity",
  na.rm = FALSE,
  ...
)
}
\arguments{
\item{fitted_values}{fitted values from a regression function using the
observed data; a list of length V, where each object is a set of
predictions on the validation data, or a vector of the same length as \code{y}.}

\item{y}{the observed outcome.}

\item{full_y}{the observed outcome (from the entire dataset, for
cross-fitted estimates).}

\item{folds}{the cross-validation folds for the observed data.}

\item{type}{which parameter are you estimating (defaults to \code{r_squared},
for R-squared-based variable importance)?}

\item{C}{the indicator of coarsening (1 denotes observed, 0 denotes
unobserved).}

\item{Z}{either \code{NULL} (if no coarsening) or a matrix-like object
containing the fully observed data.}

\item{folds_Z}{either the cross-validation folds for the observed data
(no coarsening) or a vector of folds for the fully observed data Z.}

\item{ipc_weights}{weights for inverse probability of coarsening (e.g.,
inverse weights from a two-phase sample) weighted estimation. Assumed to be
already inverted (i.e., ipc_weights = 1 / [estimated probability weights]).}

\item{ipc_fit_type}{if "external", then use \code{ipc_eif_preds}; if "SL",
fit a SuperLearner to determine the correction to the efficient
influence function.}

\item{ipc_eif_preds}{if \code{ipc_fit_type = "external"}, the fitted values
from a regression of the full-data EIF on the fully observed
covariates/outcome; otherwise, not used.}

\item{ipc_est_type}{IPC correction, either \code{"ipw"} (for classical
inverse probability weighting) or \code{"aipw"} (for augmented inverse
probability weighting; the default).}

\item{scale}{if doing an IPC correction, then the scale that the correction
should be computed on (e.g., "identity"; or "logit" to logit-transform,
apply the correction, and back-transform).}

\item{na.rm}{logical; should NA's be removed in computation?
(defaults to \code{FALSE})}

\item{...}{other arguments to SuperLearner, if \code{ipc_fit_type = "SL"}.}
}
\value{
The estimated measure of predictiveness.
}
\description{
Compute nonparametric estimates of the chosen measure of predictiveness.
}
\details{
See the paper by Williamson, Gilbert, Simon, and Carone for more
  details on the mathematics behind this function and the definition of the
  parameter of interest.  If sample-splitting is also requested
   (recommended, since in this case inferences
   will be valid even if the variable has zero true importance), then the
   prediction functions are trained as if \eqn{2K}-fold cross-validation were run,
   but are evaluated on only \eqn{K} sets (independent between the full and
   reduced nuisance regression).
}
