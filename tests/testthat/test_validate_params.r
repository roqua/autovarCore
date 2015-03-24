context('validate_params')

testdata_raw_dataframe <- function() {
  data.frame(id=rep(1, times=5),
             tijdstip=c(1, 3, 5, 6, 7),
             home=c('yes', 'no', 'yes', NA, 'yes'))
}

test_that('autovar function returns hello', {

})

test_that('default_autovar_params returns a list of expected commands', {
  default_params <- autovarCore:::default_autovar_params()
  expect_equal(class(default_params), 'list')
  expect_equal(sort(names(default_params)),
               c("criterion",
                 "imputation_iterations",
                 "measurements_per_day",
                 "significance_levels",
                 "test_names"))
})

test_that('supported_test_names returns a character vector', {
  supported_test_names <- autovarCore:::supported_test_names()
  expect_equal(class(supported_test_names), 'character')
  expect_more_than(length(supported_test_names), 3)
})

test_that('assert_param_class only accepts the expected class', {
  expected_error_message <- 'Params class should be: list'
  expect_error(autovarCore:::assert_param_class(NULL, 'list'), expected_error_message)
  expect_error(autovarCore:::assert_param_class(testdata_raw_dataframe(), 'list'), expected_error_message)
  # The statement below should not throw an error.
  expect_equal(autovarCore:::assert_param_class(list(), 'list'), NULL)
})
