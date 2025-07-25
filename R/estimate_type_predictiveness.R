#' Estimate Predictiveness Given a Type
#'
#' Estimate the specified type of predictiveness
#'
#' @param arg_lst a list of arguments; from, e.g., \code{predictiveness_measure}
#' @param type the type of predictiveness, e.g., \code{"r_squared"}
estimate_type_predictiveness <- function(arg_lst, type) {
  if (grepl("accuracy", type)) {
    est_lst <- do.call(measure_accuracy, arg_lst)
  } else if (grepl("anova", type)) {
    est_lst <- do.call(measure_anova, arg_lst)
  } else if (grepl("auc", type)) {
    est_lst <- do.call(measure_auc, arg_lst)
  } else if (grepl("average_value", type)) {
    est_lst <- do.call(measure_average_value, arg_lst)
  } else if (grepl("cross_entropy", type)) {
    est_lst <- do.call(measure_cross_entropy, arg_lst)
  } else if (grepl("deviance", type)) {
    est_lst <- do.call(measure_deviance, arg_lst)
  } else if (grepl("mse", type)) {
    est_lst <- do.call(measure_mse, arg_lst)
  } else if (grepl("r_squared", type)) {
    est_lst <- do.call(measure_r_squared, arg_lst)
  } else if (grepl("sensitivity", type)) {
    est_lst <- do.call(measure_sensitivity(, arg_lst))
  } else if (grepl("specificity", type)) {
    est_lst <- do.call(measure_specificity(, arg_lst))
  } else if (grepl("ppv", type)) {
    est_lst <- do.call(measure_ppv(, arg_lst))
  } else if (grepl("npv", type)) {
    est_lst <- do.call(measure_npv(, arg_lst))
  } else {
    stop("The requested predictiveness measure is not available in vimp.")
  }
  return(est_lst)
}
