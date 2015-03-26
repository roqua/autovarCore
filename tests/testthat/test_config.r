context('config')

# Configuration and defaults

test_that('default_autovar_params returns a list of expected commands', {
  default_params <- autovarCore:::default_autovar_params()
  expect_equal(class(default_params), 'list')
  expect_more_than(length(names(default_params)), 4)
})

test_that('supported_test_names returns a character vector', {
  supported_test_names <- autovarCore:::supported_test_names()
  expect_equal(class(supported_test_names), 'character')
  expect_more_than(length(supported_test_names), 3)
})

test_that('supported_criteria returns a character vector', {
  supported_criteria <- autovarCore:::supported_criteria()
  expect_equal(class(supported_criteria), 'character')
  expect_more_than(length(supported_criteria), 1)
})
