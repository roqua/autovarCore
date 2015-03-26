context('validate_params')

testdata_raw_dataframe <- function() {
  data.frame(id=rep(1, times=5),
             tijdstip=c(1, 3, 5, 6, 7),
             home=c('yes', 'no', 'yes', NA, 'yes'))
}


# Assertions

test_that('assert_param_class asserts that the given param is of the specified class', {
  expected_error_message <- 'Param class should be: list'
  expect_error(autovarCore:::assert_param_class(NULL, 'list'), expected_error_message)
  expect_error(autovarCore:::assert_param_class(testdata_raw_dataframe(), 'list'), expected_error_message)
  # The statement below should not throw an error.
  expect_null(autovarCore:::assert_param_class(list(), 'list'))
})

test_that('assert_param_subset throws an error if any given name is not in the allowed names', {
  expected_error_message <- "Invalid param:"
  expect_error(autovarCore:::assert_param_subset('unknown', c('a', 'b')),
               paste(expected_error_message, 'unknown'))
  expect_error(autovarCore:::assert_param_subset(c('a', 'b', 'unknown'), c('a', 'b')),
               paste(expected_error_message, 'unknown'))
  # The statements below should not throw errors.
  expect_null(autovarCore:::assert_param_subset(NULL, c('a', 'b')))
  expect_null(autovarCore:::assert_param_subset(c('a', 'b'), c('a', 'b')))
  expect_null(autovarCore:::assert_param_subset('b', c('a', 'b')))
})

test_that('assert_param_not_null throws an error when supplied a null argument', {
  expected_error_message <- 'Given param cannot be NULL'
  expect_error(autovarCore:::assert_param_not_null(NULL), expected_error_message)
  # The statements below should not throw errors.
  expect_null(autovarCore:::assert_param_not_null(1))
  expect_null(autovarCore:::assert_param_not_null('a'))
})

test_that('assert_param_integer throws an error when the given argument is not an integer', {
  expected_error_message <- 'Given param is not an integer:'
  expect_error(autovarCore:::assert_param_integer(6.7), expected_error_message)
  expect_error(autovarCore:::assert_param_integer('a'), expected_error_message)
  expect_error(autovarCore:::assert_param_integer(TRUE), expected_error_message)
  # The statements below should not throw errors.
  expect_null(autovarCore:::assert_param_integer(1))
  expect_null(autovarCore:::assert_param_integer(-7))
  expect_null(autovarCore:::assert_param_integer(0))
})

test_that('assert_param_single throws an error when the given param is not a single element', {
  expected_error_message <- 'Length of given param is not 1:'
  expect_error(autovarCore:::assert_param_single(NULL), expected_error_message)
  expect_error(autovarCore:::assert_param_single(c('a', 'b')), expected_error_message)
  # The statements below should not throw errors.
  expect_null(autovarCore:::assert_param_single(3))
  expect_null(autovarCore:::assert_param_single(-6))
  expect_null(autovarCore:::assert_param_single('b'))
  expect_null(autovarCore:::assert_param_single('ando'))
  expect_null(autovarCore:::assert_param_single(7.8))
})

test_that('assert_param_range throws an error when the given integer is not in range', {
  expected_error_message <- 'The param_name has to be an integer in range 2-10'
  expect_error(autovarCore:::assert_param_range(1, 2, 10, 'param_name'), expected_error_message)
  expect_error(autovarCore:::assert_param_range(11, 2, 10, 'param_name'), expected_error_message)
  # The statements below should not throw errors.
  expect_null(autovarCore:::assert_param_range(10, 2, 10, 'param_name'))
  expect_null(autovarCore:::assert_param_range(2, 2, 10, 'param_name'))
  expect_null(autovarCore:::assert_param_range(6, 2, 10, 'param_name'))
})


# Validation functions

test_that('validate_selected_column_names accepts only names of columns in the data frame', {
  raw_dataframe <- testdata_raw_dataframe()
  expect_error(autovarCore:::validate_selected_column_names(raw_dataframe, NULL),
               "Given param cannot be NULL")
  expect_error(autovarCore:::validate_selected_column_names(raw_dataframe,
                                                            'unknown'),
               "Invalid selected column name: unknown")

  expect_error(autovarCore:::validate_selected_column_names(raw_dataframe,
                                                            c('tijdstip', 'unknown')),
               "Invalid selected column name: unknown")
  # The statements below should not throw errors.
  expect_equal(autovarCore:::validate_selected_column_names(raw_dataframe,
                                                            'tijdstip'),
               'tijdstip')
  expect_equal(autovarCore:::validate_selected_column_names(raw_dataframe,
                                                            c('tijdstip', 'home', 'id')),
               c('tijdstip', 'home', 'id'))
})

test_that('validate_significance_levels returns the input sorted decreasingly', {
  expect_equal(autovarCore:::validate_significance_levels(testdata_raw_dataframe(),
                                                          c(0.5, 0.3, 0.7)),
                c(0.7, 0.5, 0.3))
})

test_that('validate_significance_levels accepts only numeric vectors', {
  raw_dataframe <- testdata_raw_dataframe()
  expect_error(autovarCore:::validate_significance_levels(raw_dataframe, NULL),
               "Given param cannot be NULL")
  expect_error(autovarCore:::validate_significance_levels(raw_dataframe, c('a', 'b', 'c')),
               "Param class should be: numeric")
  # The statement below should not throw an error.
  expect_equal(autovarCore:::validate_significance_levels(testdata_raw_dataframe(), 0.7),
                0.7)
})

test_that('validate_test_names accepts only supported test names', {
  raw_dataframe <- testdata_raw_dataframe()
  expect_error(autovarCore:::validate_test_names(raw_dataframe, c('portmanteau', 'unknown')),
               "Unsupported test name: unknown")
  expect_error(autovarCore:::validate_test_names(raw_dataframe, 'unknown'),
               "Unsupported test name: unknown")
  # The statements below should not throw errors.
  expect_null(autovarCore:::validate_test_names(raw_dataframe, NULL))
  expect_equal(autovarCore:::validate_test_names(raw_dataframe, c('portmanteau', 'skewness')),
               c('portmanteau', 'skewness'))
  expect_equal(autovarCore:::validate_test_names(raw_dataframe, 'kurtosis'),
               'kurtosis')
})

test_that('validate_criterion accepts only supported criteria', {
  raw_dataframe <- testdata_raw_dataframe()
  expect_error(autovarCore:::validate_criterion(raw_dataframe, NULL),
               "Given param cannot be NULL")
  expect_error(autovarCore:::validate_criterion(raw_dataframe, 'unknown'),
               "Unsupported criterion: unknown")
  expect_error(autovarCore:::validate_criterion(raw_dataframe, c('AIC', 'BIC')),
               "Length of given param is not 1:")
  # The statement below should not throw an error.
  expect_equal(autovarCore:::validate_criterion(raw_dataframe, 'AIC'),
               'AIC')
  expect_equal(autovarCore:::validate_criterion(raw_dataframe, 'BIC'),
               'BIC')
})

test_that('validate_imputation_iterations accepts only an integer in range', {
  raw_dataframe <- testdata_raw_dataframe()
  expect_error(autovarCore:::validate_imputation_iterations(raw_dataframe, NULL),
               "Given param cannot be NULL")
  expect_error(autovarCore:::validate_imputation_iterations(raw_dataframe, c(2, 3)),
               "Length of given param is not 1:")
  expect_error(autovarCore:::validate_imputation_iterations(raw_dataframe, list(a=2, b=3)),
               "Length of given param is not 1:")
  expect_error(autovarCore:::validate_imputation_iterations(raw_dataframe, 3.5),
               "Given param is not an integer:")
  expect_error(autovarCore:::validate_imputation_iterations(raw_dataframe, 'hoi'),
               "Given param is not an integer:")
  expect_error(autovarCore:::validate_imputation_iterations(raw_dataframe, 0),
               "The number of imputation iterations has to be an integer in range 1-500")
  expect_error(autovarCore:::validate_imputation_iterations(raw_dataframe, 501),
               "The number of imputation iterations has to be an integer in range 1-500")
  # The statements below should not throw errors.
  expect_equal(autovarCore:::validate_imputation_iterations(raw_dataframe, 1), 1)
  expect_equal(autovarCore:::validate_imputation_iterations(raw_dataframe, 500), 500)
  expect_equal(autovarCore:::validate_imputation_iterations(raw_dataframe, 376), 376)
})

test_that('validate_measurements_per_day accepts only an integer in range', {
  raw_dataframe <- testdata_raw_dataframe()
  expect_error(autovarCore:::validate_measurements_per_day(raw_dataframe, NULL),
               "Given param cannot be NULL")
  expect_error(autovarCore:::validate_measurements_per_day(raw_dataframe, c(2, 3)),
               "Length of given param is not 1:")
  expect_error(autovarCore:::validate_measurements_per_day(raw_dataframe, list(a=2, b=3)),
               "Length of given param is not 1:")
  expect_error(autovarCore:::validate_measurements_per_day(raw_dataframe, 3.5),
               "Given param is not an integer:")
  expect_error(autovarCore:::validate_measurements_per_day(raw_dataframe, 'hoi'),
               "Given param is not an integer:")
  expect_error(autovarCore:::validate_measurements_per_day(raw_dataframe, -1),
               "The number of measurements per day has to be an integer in range 0-16")
  expect_error(autovarCore:::validate_measurements_per_day(raw_dataframe, 17),
               "The number of measurements per day has to be an integer in range 0-16")
  # The statements below should not throw errors.
  expect_equal(autovarCore:::validate_measurements_per_day(raw_dataframe, 0), 0)
  expect_equal(autovarCore:::validate_measurements_per_day(raw_dataframe, 13), 13)
  expect_equal(autovarCore:::validate_measurements_per_day(raw_dataframe, 16), 16)
})
