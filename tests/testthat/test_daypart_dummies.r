context('daypart_dummies')

test_that('daypart_dummies returns NULL for 0 or 1 measurements per day', {
  expect_null(autovarCore:::daypart_dummies(40, 0))
  expect_null(autovarCore:::daypart_dummies(13, 1))
})

test_that('daypart_dummies calls seasonal_dummy_column correctly', {
  called_count <<- 0
  column_names <- c('dailymeas_1', 'dailymeas_2', 'dailymeas_3', 'dailymeas_4')
  expected_result <- matrix(1,
                            nrow = 2,
                            ncol = 4,
                            dimnames = list(NULL, column_names))
  with_mock(
    `autovarCore:::seasonal_dummy_column` = function(...) {
      called_count <<- called_count + 1
      c(1, 1)
    },
    expect_equal(autovarCore:::daypart_dummies(40, 5),
                 expected_result)
  )
  expect_equal(called_count, 4)
  rm(list = 'called_count', pos = '.GlobalEnv')
})

test_that('daypart_dummies calls dummy_column_names correctly', {
  calling_arg <<- NULL
  called_count <<- 0
  column_names <- c('a', 'b')
  expected_result <- matrix(NA,
                            nrow = 5,
                            ncol = 2,
                            dimnames = list(NULL, column_names))
  expected_result[, 1] <- c(1, 0, 0, 1, 0)
  expected_result[, 2] <- c(0, 1, 0, 0, 1)
  with_mock(
    `autovarCore:::dummy_column_names` = function(x) {
      calling_arg <<- x
      called_count <<- called_count + 1
      c('a', 'b')
    },
    expect_equal(autovarCore:::daypart_dummies(5, 3),
                 expected_result)
  )
  expect_equal(called_count, 1)
  expect_equal(calling_arg, 2)
  rm(list = c('called_count', 'calling_arg'), pos = '.GlobalEnv')
})

test_that('daypart_dummies returns the correct result', {
  column_names <- c('dailymeas_1', 'dailymeas_2', 'dailymeas_3')
  expected_result <- matrix(NA,
                            nrow = 9,
                            ncol = 3,
                            dimnames = list(NULL, column_names))
  expected_result[, 1] <- c(1, 0, 0, 0, 1, 0, 0, 0, 1)
  expected_result[, 2] <- c(0, 1, 0, 0, 0, 1, 0, 0, 0)
  expected_result[, 3] <- c(0, 0, 1, 0, 0, 0, 1, 0, 0)
  expect_equal(autovarCore:::daypart_dummies(9, 4),
               expected_result)
})


test_that('seasonal_dummy_column works with different lengths', {
  expect_equal(autovarCore:::seasonal_dummy_column(5, 3, 0),
               c(1, 0, 0, 1, 0))
  expect_equal(autovarCore:::seasonal_dummy_column(6, 3, 0),
               c(1, 0, 0, 1, 0, 0))
  expect_equal(autovarCore:::seasonal_dummy_column(7, 3, 0),
               c(1, 0, 0, 1, 0, 0, 1))
  expect_equal(autovarCore:::seasonal_dummy_column(3, 3, 0),
               c(1, 0, 0))
  expect_equal(autovarCore:::seasonal_dummy_column(2, 3, 0),
               c(1, 0))
  expect_equal(autovarCore:::seasonal_dummy_column(1, 3, 0),
               1)
})

test_that('seasonal_dummy_column works with different offsets', {
  expect_equal(autovarCore:::seasonal_dummy_column(5, 3, 1),
               c(0, 1, 0, 0, 1))
  expect_equal(autovarCore:::seasonal_dummy_column(10, 5, 2),
               c(0, 0, 1, 0, 0, 0, 0, 1, 0, 0))
})

test_that('seasonal_dummy_column works with different periods', {
  expect_equal(autovarCore:::seasonal_dummy_column(5, 2, 0),
               c(1, 0, 1, 0, 1))
  expect_equal(autovarCore:::seasonal_dummy_column(7, 7, 1),
               c(0, 1, 0, 0, 0, 0, 0))
  expect_equal(autovarCore:::seasonal_dummy_column(9, 4, 0),
               c(1, 0, 0, 0, 1, 0, 0, 0, 1))
})


test_that('dummy_column_names works with just one column', {
  expected_result <- 'dailymeas_1'
  expect_equal(autovarCore:::dummy_column_names(1), expected_result)
})

test_that('dummy_column_names works with more than just one column', {
  expected_result <- c('dailymeas_1', 'dailymeas_2', 'dailymeas_3')
  expect_equal(autovarCore:::dummy_column_names(3), expected_result)
})
