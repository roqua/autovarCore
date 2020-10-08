#' Return the best VAR models found for a time series data set
#'
#' This function evaluates possible VAR models for the given time series data set and returns a sorted list of the best models found. The first item in this list is the "best model" found.
#'
#' AutovarCore evaluates eight kinds of VAR models: models with and without log transforming the data, lag 1 and lag 2 models, and with and without weekday dummy variables. For the lag 2 models, all cross-lagged relations are constrained. For each of these 8 model configurations, we evaluate all possible combinations for including outlier dummies (at 2.5x the standard deviation of the residuals) and retain the best model (the procedure for selecting the best model is described in more detail below).
#'
#' These eight models are further reduced to four models by determining whether adding weekday dummies improves the model fit. AutovarCore does so based first on the significance bucket (determined by the outcomes of the residual tests) and secondly on the AIC/BIC score. If the best model found with weekday dummies is a "better model" than the best model found without weekday dummies, then AutovarCore includes the model with weekday dummies and discards the one without weekday dummies. Otherwise, AutovarCore includes only the model without weekday dummies and discards the one with weekday dummies. Thus, the comparison between models with and without weekday dummies is based on two steps:
#'
#' \enumerate{
#' \item We first consider the significance bucket. If the two models being compared are in different significance buckets, AutovarCore chooses the one with the highest significance bucket, and otherwise proceeds to step 2. The significance buckets are formed between each of the (decreasingly sorted) specified \code{significance_levels} in the parameters to the autovar function call. For example, if the \code{signifance_levels} are \code{c(0.05, 0.01, 0.005)}, then the significance buckets are \code{(0.05 <= x)}, \code{(0.01 <= x < 0.05)}, \code{(0.005 <= x < 0.01)}, and \code{(x < 0.005)}. The metric used to place a model into a bucket is the maximum p-level at which all residual tests will still pass ("pass" meaning not invalidating the model assumptions of independence, homoscedasticity, and normality).
#'
#' \item When the significance bucket for the two models being compared is the same, AutovarCore selects the model with the lowest AIC/BIC score. Whether the AIC or BIC is used here depends on the \code{criterion} option specified in the parameters to the autovar function call.
#'}
#'
#' The result of this procedure is four models: models with and without log transforming the data, and lag 1 and lag 2 models. Next, AutovarCore will determine whether the models with lag 1 or the models with lag 2 are best, and sort the models accordingly. This comparison is again based firstly on the significance bucket. If the significance bucket is the same, it proceeds to the next step, which in this case is the number of outlier dummy variables; the model with the fewest outlier dummy variables is considered the best. If the number of outlier dummy variables is the same, it proceeds to the third step, in which AutovarCore prefers the model with the lowest AIC/BIC score. This procedure results in two sorted lists of models, one list with models without logtransformation, one list with models with logtransformation.
#'
#' In the final step, AutovarCore merges the sorted lists of models with and without logtransformation. To this end, it first compares the best model of the set without logtransformation with the best logtransformed model. It will sort these models based on the significance bucket first and the AIC/BIC score secondly. After finding the best model, it is removed from its list, and the then-best models are compared. This process repeats itself until both lists are empty. The result of this procedure is a final sorted list of four models (with the best model listed first).
#'
#' The reason for the different sorting algorithms is that in some cases we want to select the model with the fewest outlier dummy columns (i.e., the model that retains most of the original data), while in other cases we know that a certain operation (such as adding weekday dummies or logtransforming the data set) will affect the amount of dummies in the model and so a fair comparison would exclude this property. For example, we do not compare the number of outlier columns in the final step because this would have likely favored logtransformed models over models without logtransform, as logtransformations typically have the effect of reducing the outliers of a sample.
#'
#' Outliers are, for each variable, the measurements at >2.5 times the standard deviation away from the mean of the residuals or of the squared residuals. Outlier dummy variables are split up such that each time point that is considered an outlier has its own dummy outlier variable and adds one to the count of outlier columns. Checks are in place to ensure that a time point identified as an outlier by multiple variables only adds a single dummy outlier column to the equation. For the count of outlier columns, day-part dummies do not add to the count. This is because when they are included, they are included for each model and thus never have any discriminatory power.
#'
#'
#' We are able to compare the AIC/BIC scores of logtransformed and nonlogtransformed models fairly because we compensate the AIC/BIC scores to account for the effect of the logtransformation. We compensate for the logtransformation by adjusting the loglikelihood score of the logtransformed models in the calculation of their AIC/BIC scores (by subtracting the sum of the logtransformed data).
#' @param raw_dataframe The raw, unimputed data frame. This can include columns other than the \code{selected_column_names}, as those may be helpful for the imputation. The measurements in the dataframe are expected to be sorted by time, and to be sequential. Missed measurements should be encoded as rows of \code{NA} values and will be imputed by EM imputation.
#' @param selected_column_names The endogenous variables in the models, specified as a vector of character strings. This argument is required. The selected column names should be a subset of the column names of \code{raw_dataframe}.
#' @param significance_levels The significance levels used for evaluating the significance of the residual tests. The variable \code{significance_levels} is a vector with descending p values that indicate cut-offs placing models in different buckets. If it is not specified, this parameter defaults to \code{c(0.05, 0.01, 0.005)}. More, fewer, and/or different significance levels can be specified. In practice, specifying more significance levels gives more weight to the outcomes of the residual tests, while having fewer significance levels gives more weight to the AIC/BIC scores and the number of dummy variables. If a test for a model has a lower p-level than the minimum specified significance level, it can still be returned by the program, but it will be assigned the special significance bucket \code{0}.
#' @param test_names The residual tests that should be performed, specified as a vector of character strings. If not specified, this parameter defaults to \code{c('portmanteau', 'portmanteau_squared', 'skewness')}, which are used to test the assumptions of independence, homoscedasticity, and normality, respectively. The possible tests are \code{c('portmanteau', 'portmanteau_squared', 'skewness', 'kurtosis', 'joint_sktest')}. In addition to the residual tests, please note that the Eigenvalue stability test is always performed.
#' @param criterion The information criterion used to sort the models. Valid options are 'BIC' (the default) or 'AIC'.
#' @param imputation_iterations The amount of times the Amelia imputation should be averaged over. The default value for this parameter is \code{100}. For details on the Amelia imputation, please see \url{https://r.iq.harvard.edu/docs/amelia/amelia.pdf}.
#' @param measurements_per_day The number of measurements per day in the time series data. The default value for this parameter is \code{1}. All models considered include day-part dummy variables if there are multiple measurements per day. If this value is \code{0}, then daypart- and weekday dummies variables are not included for any models.
#' @return A sorted list of the best models found. A "model" is a list with the properties \code{logtransformed}, \code{lag}, \code{varest}, \code{model_score}, \code{bucket}, and \code{nr_dummy_variables}. The number of models returned is at most four. In rare cases, where the Eigenvalue stability test fails for multiple models, a list with fewer than four models is returned. When the Eigenvalue test fails for all tested models (which is unlikely to happen in practice), an empty \code{list()} is returned.
#' @examples
#' \dontrun{
#' data_matrix <- matrix(nrow = 40, ncol = 3)
#' data_matrix[, ] <- runif(ncol(data_matrix) * nrow(data_matrix), 1, nrow(data_matrix))
#' while (sum(is.na(data_matrix)) == 0)
#'   data_matrix[as.logical(round(runif(ncol(data_matrix) * nrow(data_matrix), -0.3, 0.7)))] <- NA
#' colnames(data_matrix) <- c('rumination', 'happiness', 'activity')
#' dataframe <- as.data.frame(data_matrix)
#' autovar(dataframe, selected_column_names = c('rumination', 'happiness'),
#'                    significance_levels = c(0.05, 0.01, 0.005),
#'                    test_names = c('portmanteau',
#'                                   'portmanteau_squared',
#'                                   'skewness'),
#'                    criterion = 'AIC',
#'                    imputation_iterations = 100,
#'                    measurements_per_day = 1)
#' }
#' @importFrom parallel detectCores
#' @importFrom parallel makeCluster
#' @importFrom parallel clusterMap
#' @importFrom parallel stopCluster
#' @importFrom stats residuals
#' @useDynLib autovarCore
#' @export
autovar <- function(raw_dataframe, selected_column_names, significance_levels = c(0.05, 0.01, 0.005),
                    test_names = c('portmanteau', 'portmanteau_squared', 'skewness'),
                    criterion = 'BIC', imputation_iterations = 100, measurements_per_day = 1) {
  data_matrix <- validate_raw_dataframe(raw_dataframe)
  params <- validate_params(data_matrix, list(selected_column_names = selected_column_names,
                                              significance_levels = significance_levels,
                                              test_names = test_names,
                                              criterion = criterion,
                                              imputation_iterations = imputation_iterations,
                                              measurements_per_day = measurements_per_day))
  data_matrix <- impute_datamatrix(data_matrix,
                                   params$measurements_per_day,
                                   params$imputation_iterations)
  significance_buckets <- c(params$significance_levels, 0)
  cluster <- makeCluster(min(detectCores(), 8),
                         type = "PSOCK",
                         useXDR = FALSE,
                         methods = FALSE)
  model_vector <- clusterMap(cluster, evaluate_model_config, 0:7,
                             MoreArgs = list(endo_matrix = data_matrix[, params$selected_column_names],
                                             test_names = params$test_names,
                                             criterion = params$criterion,
                                             significance_buckets = significance_buckets,
                                             measurements_per_day = params$measurements_per_day),
                             SIMPLIFY = FALSE, USE.NAMES = FALSE)
  stopCluster(cluster)
  # For two otherwise identical models, determine if it is best to
  # use weekday dummies or to exclude them.
  returned_models <- list()
  for (model_index in seq(1, length(model_vector), 2)) {
    model_a <- model_vector[[model_index]]
    model_b <- model_vector[[model_index + 1]]
    if (is.null(model_a)) {
      returned_models <- c(returned_models, list(model_b))
    } else if (is.null(model_b)) {
      returned_models <- c(returned_models, list(model_a))
    } else {
      # Compare with models with/without weekday dummies, here we just look at what gives the better model.
      # Here we are not necessarily interested in which model needs more or fewer outliers, since that
      # is affected by the inclusion of weekday dummies.
      returned_models <- c(returned_models, list(compete(model_a, model_b, FALSE)))
    }
  }
  # Compare with models of other lags, here we are interested in which lag needs fewer dummies.
  # The assumption here is that if weekly cyclicity is present, it would be present on models
  # of all lags, and therefore what we are comparing here is merely which lag needs fewer outlier
  # dummies to find a model in the same significance bucket.
  non_logtransformed_models <- list() # TODO: check this:
  for (model in returned_models[1:4]) {
    if (is.null(model)) next
    non_logtransformed_models <- insert_model_into_list(model, non_logtransformed_models, TRUE)
  }
  logtransformed_models <- list()
  for (model in returned_models[5:8]) {
    if (is.null(model)) next
    logtransformed_models <- insert_model_into_list(model, logtransformed_models, TRUE)
  }
  # Merge models with/without logtransform. Since logtransform affects outliers, don't compare those.
  # The reasoning follows from above when including weekday dummies.
  returned_models <-  merge_model_lists(non_logtransformed_models, logtransformed_models, FALSE)
  returned_models
}

evaluate_model_config <- function(model_config_mask, endo_matrix, test_names, criterion,
                                  significance_buckets, measurements_per_day) {
  number_of_endo_vars <- ncol(endo_matrix)
  all_outlier_masks <- 0:(2^(number_of_endo_vars) - 1)
  number_of_measurements <- nrow(endo_matrix)
  logtransformed <- (bitwAnd(model_config_mask, 4) != 0)
  lag <- ifelse(bitwAnd(model_config_mask, 2) != 0, 2, 1)
  use_day_dummies <- (bitwAnd(model_config_mask, 1) != 0)
  if (logtransformed)
    endo_matrix <- apply_ln_transformation(endo_matrix)
  if (needs_trend(endo_matrix, lag))
    exo_matrix <- cbind(daypart_dummies(number_of_measurements, measurements_per_day),
                        trend_columns(number_of_measurements))
  else
    exo_matrix <- daypart_dummies(number_of_measurements, measurements_per_day)
  if (use_day_dummies) {
    day_dummy_data <- day_dummies(number_of_measurements, measurements_per_day)
    if (is.null(day_dummy_data))
      return(NULL)
    exo_matrix <- cbind(exo_matrix, day_dummy_data)
  }
  outlier_dummies <- residual_outliers(residuals(run_var(endo_matrix,
                                                         exo_matrix,
                                                         lag)),
                                       number_of_measurements)
  outlier_masks <- select_valid_masks(all_outlier_masks,
                                      invalid_mask(outlier_dummies))
  best_model <- list(model_score = Inf, bucket = 0, nr_dummy_variables = Inf)
  for (outlier_mask in outlier_masks) {
    model <- evaluate_model(outlier_mask, endo_matrix, exo_matrix, lag, outlier_dummies,
                            test_names, criterion, logtransformed, significance_buckets)
    if (is.null(model)) next
    # Compare with models with more or fewer outlier dummies. Prefer models with fewer outlier dummies.
    best_model <- compete(best_model, model, TRUE)
  }
  if (!is.infinite(best_model$model_score))
    return(best_model)
  NULL
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
       bucket = significance_bucket,
       nr_dummy_variables = nr_dummy_variables(varest))
}

nr_dummy_variables <- function(varest) {
  outlier_dummies <- length(grep("^outlier_[0-9]+$", colnames(varest$datamat)))
  # Do not count day dummies:
  # day_dummies <- length(grep("^day_[0-9]+$", colnames(varest$datamat)))
  # if (day_dummies > 0)
  #   day_dummies <- 1
  # outlier_dummies + day_dummies
  outlier_dummies
}

insert_model_into_list <- function(model, model_list, compare_outliers) {
  if (length(model_list) == 0) return(list(model))
  if (challenger_wins(model, model_list[[length(model_list)]], compare_outliers))
    return(append(model_list, list(model)))
  left <- 1
  right <- length(model_list)
  while (left != right) {
    middle <- (left + right)%/%2
    if (challenger_wins(model_list[[middle]], model, compare_outliers))
      right <- middle
    else
      left <- middle + 1
  }
  append(model_list, list(model), after = left - 1)
}

merge_model_lists <- function(list_a, list_b, compare_outliers) {
  pos_a <- 1
  pos_b <- 1
  result <- list()
  len_a <- length(list_a)
  len_b <- length(list_b)
  while (pos_a <= len_a && pos_b <= len_b) {
    if (challenger_wins(list_a[[pos_a]], list_b[[pos_b]], compare_outliers)) {
      result <- append(result, list(list_b[[pos_b]]))
      pos_b <- pos_b + 1
    } else {
      result <- append(result, list(list_a[[pos_a]]))
      pos_a <- pos_a + 1
    }
  }
  if (pos_a <= len_a)
    result <- append(result, list_a[pos_a:len_a])
  if (pos_b <= len_b)
    result <- append(result, list_b[pos_b:len_b])
  result
}
