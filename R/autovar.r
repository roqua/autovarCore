#' Return a JSON array of network data
#'
#' This function finds the best VAR model for the given data set and parameters, and returns a JSON dictionary with contemporaneous relations, dynamic relations, and a top three for the dynamic relations.
#'
#' Selecting the best model happens in the following way:
#' Until the very end, we always keep track of two "best models," namely the best model of the logtransformed data and the best model of the original (non-logtransformed) data. Thus, we only compare a "logtransformed" model with another "logtransformed" model or a "nonlogtransformed" model with another "nonlogtransformed" model. For these comparisons, the following rules apply: \enumerate{
#' \item For a model to be considered, it has to pass the Eigenvalue stability test. Any model that does not pass this test is immediately discarded.
#' \item The significance bucket is the most important. If the two models being compared are in different significance buckets, choose the one with the highest significance bucket, otherwise proceed to step 3.
#'
#' The significance buckets are formed between each of the (decreasingly sorted) specified \code{significance_levels} in the parameters to the autovar function call. For example, if the \code{signifance_levels} are \code{c(0.05, 0.01, 0.005)}, then the significance buckets are \code{(0.05 <= x), (0.01 <= x < 0.05), (0.005 <= x < 0.01),} and \code{(x < 0.005)}. The metric used to place a model into a bucket is the maximum p-level that can be chosen as cut-off for determining whether an outcome is statistically significant such that all residual tests will still pass ("pass" meaning not invalidating the assumption that the residuals are normally distributed). In other words: it is the minimum p-value of all three residual tests of all endogenous variables in the model.
#' \item If the two models being compared are in the same significance bucket, the number of outlier columns is most important. If the two models being compared have a different amount of outlier columns, choose the one with the least amount of outlier columns, otherwise proceed to step 4.
#'
#' For this count of outlier columns, the following rules apply: \itemize{
#' \item Day-part dummies do not add to the count. This is because when they are included, they are included for each model and thus never have any discriminatory power.
#' \item Day dummies count as one outlier column in total (so including day dummies will add one outlier column). This is because we do not necessarily want to punish models if the data happens to exhibit weekly cyclicity, but models that do not need the day dummies and are equally likely should be preferred.
#' \item Outlier dummy variables are split up such that each time point that is considered an outlier has its own dummy outlier variable and adds one to the count of outlier columns. The outliers are, for each variable, the measurements at >2.5 times the standard deviation away from the mean of the residuals or of the squared residuals. Checks are in place to ensure that a time point identified as an outlier by multiple variables only adds a single dummy outlier column to the equation.
#' }
#' \item When the bucket and number of outlier columns for the two models being compared are the same, select the one with the lowest AIC/BIC score. Whether the AIC or BIC is used here depends on the  \code{criterion} option specified in the parameters to the autovar function call.
#' }
#' In the end, we should have one best logtransformed model and one best nonlogtransformed model. We then compare these two models in the same way as we have compared all other models up to this point with one exception: we do not compare the number of outlier columns. Comparing the number of outliers would have likely favored logtransformed models over models without logtransform, as logtransformations typically have the effect of reducing the outliers of a sample. Thus, we instead compare by bucket first and AIC/BIC score second.
#'
#' We are able to compare the AIC/BIC scores of logtransformed and nonlogtransformed models fairly because we compensate the AIC/BIC scores to account for the effect of the logtransformation. We compensate for the logtransformation by adjusting the loglikelihood score of the logtransformed models in the calculation of their AIC/BIC scores (by subtracting the sum of the logtransformed data).
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
                                                   logtransformed = use_logtransform,
                                                   significance_buckets = significance_buckets),
                                   SIMPLIFY = FALSE, USE.NAMES = FALSE)
        for (model in model_vector) {
          if (is.null(model)) next
          #print(model)
          # for each model in model_vector, best <- compete(best, model)
          # TODO: add code
        }
      }
    }
  }
  stopCluster(cluster)
  "Hello world!"
}

evaluate_model <- function(outlier_mask, endo_matrix, exo_matrix, lag, outlier_dummies,
                           test_names, criterion, logtransformed, significance_buckets) {
  if (outlier_mask != 0) {
    selected_column_indices <- selected_columns(outlier_mask)
    exploded_outlier_dummies <- explode_dummies(as.matrix(outlier_dummies[, selected_column_indices]))
    exo_matrix <- cbind(exo_matrix, exploded_outlier_dummies)
  }
  varest <- run_var(endo_matrix, exo_matrix, lag)
  if (!model_is_stable(varest))
    return(NULL)
  significance_p_values <- run_tests(varest, test_names)
  model_significance <- min(significance_p_values)
  score <- model_score(varest, criterion, logtransformed)
  significance_bucket <- 0
  for (bucket in significance_buckets) {
    if (model_significance < bucket) next
    significance_bucket <- bucket
    break
  }
  list(logtransformed = logtransformed,
       lag = lag,
       varest = varest,
       model_score = score,
       bucket = significance_bucket)
}
