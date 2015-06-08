context('compete')

test_that('compete calls its subfunctions correctly', {
  called_count <<- 0
  best <- list(bucket = 0.05, model_score = 100,
               varest = list(datamat = matrix(nrow = 40, ncol = 3,
                                              dimnames = list(NULL, c('a', 'day_1', 'day_2')))))
  challenger <- list(bucket = 0.05, model_score = 99,
                     varest = list(datamat = matrix(nrow = 40, ncol = 3,
                                                    dimnames = list(NULL, c('a', 'day_1', 'day_2')))))
  with_mock(
    `autovarCore:::nr_dummy_variables` = function(...) {
      called_count <<- called_count + 1
      called_count
    },
    expect_equal(autovarCore:::compete(best, challenger, TRUE), best)
  )
  expect_equal(called_count, 2)
  called_count <<- 0
  best <- list(bucket = 0.05, model_score = 100,
               varest = list(datamat = matrix(nrow = 40, ncol = 3,
                                              dimnames = list(NULL, c('a', 'day_1', 'day_2')))))
  challenger <- list(bucket = 0.05, model_score = 99,
                     varest = list(datamat = matrix(nrow = 40, ncol = 3,
                                                    dimnames = list(NULL, c('a', 'day_1', 'day_2')))))
  with_mock(
    `autovarCore:::nr_dummy_variables` = function(...) {
      called_count <<- called_count + 1
      called_count
    },
    expect_equal(autovarCore:::compete(best, challenger, FALSE), challenger)
  )
  expect_equal(called_count, 0)
  rm(list = c('called_count'), pos = '.GlobalEnv')
})

test_that('compete returns the model with the highest bucket', {
  best <- list(bucket = 0.05)
  challenger <- list(bucket = 0.01)
  expect_equal(autovarCore:::compete(best, challenger, TRUE), best)
})

test_that('compete returns the model with the least outlier columns if buckets are equal', {
  best <- list(bucket = 0.05, model_score = 99,
               varest = list(datamat = matrix(nrow = 40, ncol = 3,
                                              dimnames = list(NULL, c('a', 'outlier_1', 'outlier_2')))))
  challenger <- list(bucket = 0.05, model_score = 100,
                     varest = list(datamat = matrix(nrow = 40, ncol = 3,
                                                    dimnames = list(NULL, c('a', 'day_1', 'day_2')))))
  expect_equal(autovarCore:::compete(best, challenger, TRUE), challenger)
})

test_that('compete otherwise returns the model with lowest model_score', {
  best <- list(bucket = 0.05, model_score = 100,
               varest = list(datamat = matrix(nrow = 40, ncol = 3,
                                              dimnames = list(NULL, c('a', 'day_1', 'day_2')))))
  challenger <- list(bucket = 0.05, model_score = 99,
               varest = list(datamat = matrix(nrow = 40, ncol = 3,
                                              dimnames = list(NULL, c('a', 'day_1', 'day_2')))))
  expect_equal(autovarCore:::compete(best, challenger, TRUE), challenger)
  expect_equal(autovarCore:::compete(best, challenger, FALSE), challenger)
})

test_that('nr_dummy_variables calls its subfunctions correctly', {
  varest <- list(datamat = matrix(nrow = 40, ncol = 3, dimnames = list(NULL, c('a', 'day_1', 'day_2'))))
  expect_equal(autovarCore:::nr_dummy_variables(varest), 1)
  varest <- list(datamat = matrix(nrow = 40, ncol = 3, dimnames = list(NULL, c('a', 'day_1', 'c'))))
  expect_equal(autovarCore:::nr_dummy_variables(varest), 1)
  varest <- list(datamat = matrix(nrow = 40, ncol = 3, dimnames = list(NULL, c('a', 'outlier_1', 'c'))))
  expect_equal(autovarCore:::nr_dummy_variables(varest), 1)
  varest <- list(datamat = matrix(nrow = 40, ncol = 3, dimnames = list(NULL, c('a', 'outlier_2', 'outlier_4'))))
  expect_equal(autovarCore:::nr_dummy_variables(varest), 2)
  varest <- list(datamat = matrix(nrow = 40, ncol = 3, dimnames = list(NULL, c('day_2', 'outlier_2', 'outlier_4'))))
  expect_equal(autovarCore:::nr_dummy_variables(varest), 3)
  varest <- list(datamat = matrix(nrow = 40, ncol = 3, dimnames = list(NULL, c('day_2', 'day_4', 'outlier_3'))))
  expect_equal(autovarCore:::nr_dummy_variables(varest), 2)
})

test_that('nr_dummy_variables works with 0 dummy variables', {
  varest <- list(datamat = matrix(nrow = 40, ncol = 3, dimnames = list(NULL, c('a', 'b', 'c'))))
  expect_equal(autovarCore:::nr_dummy_variables(varest), 0)
})
