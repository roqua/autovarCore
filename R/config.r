# Configuration and defaults

default_autovar_params <- function() {
  list(significance_levels = c(0.05, 0.01, 0.005),
       test_names = c('portmanteau', 'portmanteau_squared', 'skewness'),
       criterion = 'AIC',
       imputation_iterations = 30,
       measurements_per_day = 1)
}

supported_test_names <- function() {
  c('portmanteau',
    'portmanteau_squared',
    'skewness',
    'kurtosis')
}

supported_criteria <- function() {
  c('AIC',
    'BIC')
}

p_level_for_trend_significance <- function() {
  0.05
}
