#' Validates the params given to the autovar function
#'
#' This function uses a list of default params that may be overwritten the \code{params} argument. \code{stop()} errors are thrown when invalid params are supplied.
#' @param raw_dataframe The raw, unimputed data frame. This parameter is supplied so that we can verify the selected column names.
#' @param params A \code{list} with the following named entries: \itemize{
#' \item \code{selected_column_names} - The endogenous variables in the models, specified as an array of character strings. This argument is required. The selected column names should be a subset of the column names of \code{raw_dataframe}.
#' \item \code{significance_levels} - An array with descending p values that indicate cut-offs placing models in different buckets. If it is not specified, this parameter defaults to \code{c(0.05, 0.01, 0.005)}. For example, with the default configuration, a model whose worst (lowest) p-level for any test is 0.03 is always seen as a better model than one whose worst p-level for any test is 0.009, no matter the AIC/BIC score of that model. Also, the lowest significance level indicates the minimum p-level for any test of a valid model. Thus, if a test for a model has a lower p-level than the minimum specified significance level, it is considered invalid.
#' \item \code{test_names} - The residual tests that should be performed, specified as an array of character strings. If not specified, this parameter defaults to \code{c('portmanteau', 'portmanteau_squared', 'skewness')}. The possible tests are \code{c('portmanteau', 'portmanteau_squared', 'skewness', 'kurtosis')}. In addition to the residual tests, please note that the Eigenvalue stability test is always performed.
#' \item \code{criterion} - The information criterion used to sort the models. Valid options are 'AIC' (the default) or 'BIC'.
#' \item \code{imputation_iterations} - The number of times we average over one Amelia call for imputing the data set. Since one Amelia call averages over five imputations on its own, the actual number of imputations is five times the number specified here. The default value for this parameter is \code{30}.
#' \item \code{measurements_per_day} - The number of measurements per day in the time series data. The default value for this parameter is \code{1}.
#' }
#' @return A list with augmented params.
#' @examples
#' # Here we only overwrite the imputation iterations, the rest is left at default.
#' autovarCore:::validate_params(list(imputation_iterations=20))
#' @export
validate_params <- function(raw_dataframe, params) {
  "Hello otherworld!"
}
