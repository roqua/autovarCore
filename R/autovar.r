#' Return a JSON array of network data
#'
#' This function finds the best VAR model for the given data set and parameters, and returns a JSON dictionary with contemporaneous relations, dynamic relations, and a top three for the dynamic relations.
#' @param raw_dataframe The raw, unimputed data frame. This can include columns other than the \code{selected_column_names}, as those may be helpful for the imputation.
#' @param params A \code{list} with the following named entries: \itemize{
#' \item \code{selected_column_names} - The endogenous variables in the models, specified as a vector of character strings. This argument is required. The selected column names should be a subset of the column names of \code{raw_dataframe}.
#' \item \code{significance_levels} - A vector with descending p values that indicate cut-offs placing models in different buckets. If it is not specified, this parameter defaults to \code{c(0.05, 0.01, 0.005)}. For example, with the default configuration, a model whose worst (lowest) p-level for any test is 0.03 is always seen as a better model than one whose worst p-level for any test is 0.009, no matter the AIC/BIC score of that model. Also, the lowest significance level indicates the minimum p-level for any test of a valid model. Thus, if a test for a model has a lower p-level than the minimum specified significance level, it is considered invalid.
#' \item \code{test_names} - The residual tests that should be performed, specified as a vector of character strings. If not specified, this parameter defaults to \code{c('portmanteau', 'portmanteau_squared', 'skewness')}. The possible tests are \code{c('portmanteau', 'portmanteau_squared', 'skewness', 'kurtosis', 'joint_sktest')}. In addition to the residual tests, please note that the Eigenvalue stability test is always performed.
#' \item \code{criterion} - The information criterion used to sort the models. Valid options are 'AIC' (the default) or 'BIC'.
#' \item \code{imputation_iterations} - The number of times we average over one Amelia call for imputing the data set. Since one Amelia call averages over five imputations on its own, the actual number of imputations is five times the number specified here. The default value for this parameter is \code{30}.
#' \item \code{measurements_per_day} - The number of measurements per day in the time series data. The default value for this parameter is \code{1}. If this value is \code{0}, then daypart- and day-dummies variables are not included for any models.
#' }
#' @return JSON dictionary with two elements: \code{dynamic_network} and \code{contemporaneous_network}.
#' @examples
#' data_matrix <- matrix(nrow = 40, ncol = 3)
#' data_matrix[, ] <- runif(ncol(data_matrix) * nrow(data_matrix), 1, nrow(data_matrix))
#' while (sum(is.na(data_matrix)) == 0)
#'   data_matrix[as.logical(round(runif(ncol(data_matrix) * nrow(data_matrix), -0.3, 0.7)))] <- NA
#' colnames(data_matrix) <- c('rumination', 'happiness', 'activity')
#' dataframe <- as.data.frame(data_matrix)
#' autovar(dataframe, list(selected_column_names = c('rumination', 'happiness'),
#'                         significance_levels = c(0.05, 0.01, 0.005),
#'                         test_names = c('portmanteau',
#'                                        'portmanteau_squared',
#'                                        'skewness'),
#'                         criterion = 'AIC',
#'                         imputation_iterations = 30,
#'                         measurements_per_day = 1))
#' @export
autovar <- function(raw_dataframe, params) {
  data_matrix <- validate_raw_dataframe(raw_dataframe)
  params <- validate_params(data_matrix, params)
  data_matrix <- impute_datamatrix(data_matrix,
                                   params$measurements_per_day,
                                   params$imputation_iterations)
  number_of_measurements <- nrow(data_matrix)
  ln_data_matrix <- apply_ln_transformation(data_matrix)
  daypart_dummy_data <- daypart_dummies(number_of_measurements,
                                        params$measurements_per_day)
  day_dummy_data <- day_dummies(number_of_measurements,
                                params$measurements_per_day)
  trend_column_matrix <- trend_columns(number_of_measurements)
  number_of_endo_vars <- length(params$selected_column_names)
  cluster <- makeCluster(detectCores(),
                         type = "PSOCK",
                         useXDR = FALSE,
                         methods = FALSE)
  all_outlier_masks <- 0:(2^(number_of_endo_vars) - 1)
  significance_buckets <- c(params$significance_levels, 0)
  best_model <- list(model_score = Inf, bucket = 0)
  for (use_logtransform in c(FALSE, TRUE)) {
    if (use_logtransform)
      endo_matrix <- ln_data_matrix[, params$selected_column_names]
    else
      endo_matrix <- data_matrix[, params$selected_column_names]
    for (use_daydummies in c(FALSE, TRUE)) {
      if (use_daydummies) {
        if (is.null(day_dummy_data)) next
        seasonal_dummies <- cbind(daypart_dummy_data, day_dummy_data)
      } else {
        seasonal_dummies <- daypart_dummy_data
      }
      for (lag in 1:2) {
        if (needs_trend(endo_matrix, lag))
          exo_matrix <- cbind(seasonal_dummies, trend_column_matrix)
        else
          exo_matrix <- seasonal_dummies
        outlier_dummies <- residual_outliers(residuals(run_var(endo_matrix,
                                                               exo_matrix,
                                                               lag)),
                                             number_of_measurements)
        outlier_masks <- select_valid_masks(all_outlier_masks,
                                            invalid_mask(outlier_dummies))
        model_vector <- clusterMap(cluster, evaluate_model, outlier_masks,
                                   MoreArgs = list(endo_matrix = endo_matrix,
                                                   exo_matrix = exo_matrix,
                                                   lag = lag,
                                                   outlier_dummies = outlier_dummies,
                                                   test_names = params$test_names,
                                                   criterion = params$criterion,
                                                   use_logtansform = use_logtransform),
                                   SIMPLIFY = FALSE, USE.NAMES = FALSE)
        #print(model_vector)
        # for each model in model_vector, best <- compete(best, model)
        # next if the model is NULL
        # TODO: add code
      }
    }
  }
  stopCluster(cluster)
  "Hello world!"
}

evaluate_model <- function(outlier_mask, endo_matrix, exo_matrix, lag,
                           outlier_dummies, test_names, criterion, use_logtransform) {
  if (outlier_mask != 0) {
    selected_column_indices <- selected_columns(outlier_mask)
    exploded_outlier_dummies <- explode_dummies(as.matrix(outlier_dummies[, selected_column_indices]))
    exo_matrix <- cbind(exo_matrix, exploded_outlier_dummies)
  }
  varest <- run_var(endo_matrix, exo_matrix, lag)
  if (!model_is_stable(varest))
    return(NULL)
  # TODO: run tests and return all test values or just the minimum
  significance_p_values <- run_tests(varest, test_names)
  score <- model_score(varest, criterion, use_logtransform)
  NULL
}
