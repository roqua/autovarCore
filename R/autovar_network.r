#' Return a JSON array of network data of a fitting model
#'
#' This function calls \code{\link{autovar}} to find a fitting model for the data provided and then returns a JSON representation of the best valid model found.
#'
#'
#' @param raw_dataframe The raw, unimputed data frame. This can include columns other than the \code{selected_column_names}, as those may be helpful for the imputation.
#' @param selected_column_names The endogenous variables in the models, specified as a vector of character strings. This argument is required. The selected column names should be a subset of the column names of \code{raw_dataframe}.
#' @param significance_levels A vector with descending p values that indicate cut-offs placing models in different buckets. If it is not specified, this parameter defaults to \code{c(0.05, 0.01, 0.005)}. For example, with the default configuration, a model whose worst (lowest) p-level for any test is 0.03 is always seen as a better model than one whose worst p-level for any test is 0.009, no matter the AIC/BIC score of that model. Also, the lowest significance level indicates the minimum p-level for any test of a valid model. Thus, if a test for a model has a lower p-level than the minimum specified significance level, it is considered invalid.
#' @param test_names The residual tests that should be performed, specified as a vector of character strings. If not specified, this parameter defaults to \code{c('portmanteau', 'portmanteau_squared', 'skewness')}. The possible tests are \code{c('portmanteau', 'portmanteau_squared', 'skewness', 'kurtosis', 'joint_sktest')}. In addition to the residual tests, please note that the Eigenvalue stability test is always performed.
#' @param criterion The information criterion used to sort the models. Valid options are 'AIC' (the default) or 'BIC'.
#' @param imputation_iterations The number of times we average over one Amelia call for imputing the data set. Since one Amelia call averages over five imputations on its own, the actual number of imputations is five times the number specified here. The default value for this parameter is \code{30}.
#' @param measurements_per_day The number of measurements per day in the time series data. The default value for this parameter is \code{1}. If this value is \code{0}, then daypart- and day-dummies variables are not included for any models.
#' @param forced_variable a variable that, if not \code{NULL}, will be the target of the third connection in the top three of connections that is returned by this function.
#' @param incident_to_forced_variable a list of variables that are influenceable by the user. These are used in the top three, if \code{forced_variable} is set, to determine whether or not to show a connection as a third connection.
#' @param signs a list where keys are variable names and values are either \code{'positive'} or \code{'negative'}.
#' @param labels a list where keys are variable names and values are labels. These labels are used in the graph representation of the network.
#' @param include_model determines whether the imputed data set and coefficients of the best model should be returned in the JSON array as a fourth argument. Defaults to \code{FALSE}.
#' @return This function returns a string representing a json array with three or four elements (depending on the value of \code{include_model}). The first element is the two networks and an array of the top links.
#' @examples
#' @export
autovar_network <- function(raw_dataframe, selected_column_names, significance_levels = c(0.05, 0.01, 0.005),
                            test_names = c('portmanteau', 'portmanteau_squared', 'skewness'),
                            criterion = 'AIC', imputation_iterations = 30, measurements_per_day = 1,
                            forced_variable = NULL, incident_to_forced_variable = NULL,
                            signs = list(), labels = list(), include_model = FALSE) {
  # run autovar
  # return nil if the bucket of the best model is 0 or autovar returns an empty list
  # call convert_to_graph (split it up in separaete functions)
  # at the end, convert the result to json
  # TODO: example
}
