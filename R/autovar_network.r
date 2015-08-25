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
#' @param signs a list where keys are variable names and values are either \code{'positive'} or \code{'negative'}.
#' @param labels a list where keys are variable names and values are labels. These labels are used in the graph representation of the network.
#' @return This function returns a string representing a json array with three or four elements (depending on the value of \code{include_model}). The first element is the two networks and an array of the top links.
#' @examples
#' \dontrun{
#' data_matrix <- matrix(nrow = 40, ncol = 3)
#' data_matrix[, ] <- runif(ncol(data_matrix) * nrow(data_matrix), 1, nrow(data_matrix))
#' while (sum(is.na(data_matrix)) == 0)
#'   data_matrix[as.logical(round(runif(ncol(data_matrix) * nrow(data_matrix), -0.3, 0.7)))] <- NA
#' colnames(data_matrix) <- c('rumination', 'happiness', 'activity')
#' dataframe <- as.data.frame(data_matrix)
#' autovar_network(dataframe, selected_column_names = c('rumination', 'happiness', 'activity'),
#'                 significance_levels = c(0.05, 0.01),
#'                 test_names = c('portmanteau',
#'                                'portmanteau_squared',
#'                                'skewness'),
#'                 criterion = 'AIC',
#'                 imputation_iterations = 30,
#'                 measurements_per_day = 1,
#'                 signs = list(rumination = 'negative',
#'                              happiness = 'positive',
#'                              activity = 'positive'),
#'                 labels = list(rumination = 'Rumination',
#'                               happiness = 'Happiness',
#'                               activity = 'Activity'))
#' }
#' @export
autovar_network <- function(raw_dataframe, selected_column_names, significance_levels = c(0.05, 0.01, 0.005),
  test_names = c('portmanteau', 'portmanteau_squared', 'skewness'),
  criterion = 'AIC', imputation_iterations = 30, measurements_per_day = 1, subnet_size = 5,
  signs = list(), labels = list()) {
  imputed_dataframe <- as.data.frame(impute_datamatrix(as.matrix(raw_dataframe), measurements_per_day, 30))
  # TODO: check the given parameters
  calculate_combinations(raw_dataframe,
                         selected_column_names,
                         significance_levels,
                         test_names,
                         criterion,
                         imputation_iterations,
                         measurements_per_day,
                         subnet_size)
}

calculate_combinations <- function(raw_dataframe, selected_column_names, significance_levels,
                                   test_names, criterion, imputation_iterations, measurements_per_day, subnet_size) {
  column_count <- length(selected_column_names)
  p_counts <- matrix(0, ncol = column_count, nrow = column_count)
  p_sums <- matrix(0, ncol = column_count, nrow = column_count)
  coef_sums <- matrix(0, ncol = column_count, nrow = column_count)
  add_subnet_edges <- function(raw_dataframe, subnet_column_names, significance_levels,
                           test_names, criterion, imputation_iterations, measurements_per_day) {
    models <- autovar(raw_dataframe = raw_dataframe,
                      selected_column_names = subnet_column_names,
                      significance_levels = significance_levels,
                      test_names = test_names,
                      criterion = criterion,
                      imputation_iterations = imputation_iterations,
                      measurements_per_day = measurements_per_day)
    if (length(models) == 0 || models[[1]]$bucket == 0)
      return(NULL)
    for (idx1 in 1:length(selected_column_names)) {
      var_source <- selected_column_names[idx1]
      if (!(var_source %in% subnet_column_names)) next
      for (idx2 in 1:length(selected_column_names)) {
        if (idx1 == idx2) next
        var_target <- selected_column_names[idx2]
        if (!(var_target %in% subnet_column_names)) next
        for (model in models) {
          if (model$bucket != significance_levels[[1]]) next
          first_lag_of_source <- paste(var_source, '.l1', sep = '')
          var_coef <- coef(summary(model$varest$varresult[[var_target]]))[first_lag_of_source, ]
          p_counts[idx1, idx2] <<- p_counts[idx1, idx2] + 1
          p_sums[idx1, idx2] <<- p_sums[idx1, idx2] + var_coef[['Pr(>|t|)']]
          coef_sums[idx1, idx2] <<- coef_sums[idx1, idx2] + var_coef[['Estimate']]
        }
      }
    }
  }
  calculate_combination <- function(idx, cnt, mask) {
    if (idx == column_count) {
      if (cnt == 0) {
        result <- NULL
        for (bitpos in 0:(column_count - 1))
          if (bitwAnd(mask, bitwShiftL(1, bitpos)) != 0)
            result <- c(result, selected_column_names[bitpos + 1])
        add_subnet_edges(raw_dataframe, result, significance_levels,
                         test_names, criterion, imputation_iterations, measurements_per_day)
        return(NULL)
      }
      return(NULL)
    }
    if (cnt > 0)
      calculate_combination(idx + 1, cnt - 1, bitwOr(mask, bitwShiftL(1, idx)))
    calculate_combination(idx + 1, cnt, mask)
  }
  calculate_combination(0, subnet_size, 0)
  p_sums <- p_sums / p_counts
  coef_sums <- coef_sums / p_counts
  print(p_counts)
  print("=====")
  print(p_sums)
  print("----")
  print(coef_sums)
  result <- NULL
  for (idx1 in 1:length(selected_column_names)) {
    var_source <- selected_column_names[idx1]
    for (idx2 in 1:length(selected_column_names)) {
      if (idx1 == idx2) next
      if (p_counts[idx1, idx2] == 0) next
      var_target <- selected_column_names[idx2]
      cur_p <- p_sums[idx1, idx2]
      cur_coef <- coef_sums[idx1, idx2]
      if (cur_p <= p_level_for_granger_significance()) {
        result <- rbind(result, data.frame(source = var_source,
                                           target = var_target,
                                           coef = cur_coef,
                                           stringsAsFactors = FALSE))
      }
    }
  }
  result
}

