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
#' raw_dataframe <- data.frame(id=rep(1, times=5),
#'   tijdstip=c(1, 3, 5, 6, 7),
#'   home=c('yes', 'no', 'yes', NA, 'yes'))
#' autovarCore:::validate_params(raw_dataframe,
#'                               list(imputation_iterations=20))
#' @export
validate_params <- function(raw_dataframe, params) {
  # precondition: raw_dataframe is assumed to be a valid data set
  #               and is not validated here (it is validated in another function)
  # TODO: CHANGE TO MATRIX
  returned_params <- default_autovar_params()
  assert_param_class(params, 'list')
  assert_param_subset(names(params),
                      c(names(returned_params), 'selected_column_names'))
  if (!('selected_column_names' %in% names(params)))
    stop("selected_column_names is a required parameter")
  for (param_name in names(params)) {
    validation_function <- switch(param_name,
       selected_column_names = validate_selected_column_names,
       significance_levels = validate_significance_levels,
       test_names = validate_test_names,
       criterion = validate_criterion,
       imputation_iterations = validate_imputation_iterations,
       measurements_per_day = validate_measurements_per_day)
    returned_params[[param_name]] <- validation_function(raw_dataframe,
                                                         params[[param_name]])
  }
  returned_params
}


# Assertions

assert_param_class <- function(param, expected_class) {
  if (class(param) != expected_class)
    stop(paste("Param class should be:", expected_class))
}

assert_param_subset <- function(given_names_vector, allowed_names_vector,
                                error_message = "Invalid param:") {
  for (param_name in given_names_vector)
    if (is.null(param_name) || !(param_name %in% allowed_names_vector))
      stop(paste(error_message, param_name))
}

assert_param_not_null <- function(given_param) {
  if (is.null(given_param))
    stop("Given param cannot be NULL")
}

assert_param_integer <- function(given_param) {
  # precondition: given_param is a single element
  if (class(given_param) != 'numeric' || !(given_param%%1 == 0))
    stop(paste("Given param is not an integer:"), given_param)
}

assert_param_single <- function(given_param) {
  # precondition: given_param is not NULL
  if (length(given_param) != 1)
    stop(paste("Length of given param is not 1:", given_param))
}

assert_param_range <- function(given_param, min, max, param_name) {
  # precondition: given_param is an integer
  if (given_param < min || given_param > max)
    stop(paste("The ",
               param_name,
               " has to be an integer in range ",
               min, "-", max, sep=""))
}


# Validation functions

validate_selected_column_names <- function(raw_dataframe, given_param) {
  # precondition: raw_dataframe is a data frame
  assert_param_not_null(given_param)
  accepted_column_names <- names(raw_dataframe)
  assert_param_subset(given_param,
                      accepted_column_names,
                      "Invalid selected column name:")
  given_param
}

validate_significance_levels <- function(raw_dataframe, given_param) {
  assert_param_not_null(given_param)
  assert_param_class(given_param, 'numeric')
  sort(given_param, decreasing = TRUE)
}

validate_test_names <- function(raw_dataframe, given_param) {
  if (is.null(given_param)) return(NULL) # An empty vector of tests is allowed
  assert_param_subset(given_param,
                      supported_test_names(),
                      "Unsupported test name:")
  given_param
}

validate_criterion <- function(raw_dataframe, given_param) {
  assert_param_not_null(given_param)
  assert_param_single(given_param)
  assert_param_subset(given_param,
                      supported_criteria(),
                      "Unsupported criterion:")
  given_param
}

validate_imputation_iterations <- function(raw_dataframe, given_param) {
  assert_param_not_null(given_param)
  assert_param_single(given_param)
  assert_param_integer(given_param)
  assert_param_range(given_param, 1, 500, "number of imputation iterations")
  given_param
}

validate_measurements_per_day <- function(raw_dataframe, given_param) {
  assert_param_not_null(given_param)
  assert_param_single(given_param)
  assert_param_integer(given_param)
  # We may use the 0 value to denote that day- and daypart dummies are not to be included.
  assert_param_range(given_param, 0, 16, "number of measurements per day")
  given_param
}
