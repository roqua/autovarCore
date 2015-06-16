context('autovar')

testdata_raw_dataframe <- function() {
  data_matrix <- matrix(nrow = 40, ncol = 3)
  data_matrix[, ] <- runif(ncol(data_matrix) * nrow(data_matrix), 1, nrow(data_matrix))
  while (sum(is.na(data_matrix)) == 0)
    data_matrix[as.logical(round(runif(ncol(data_matrix) * nrow(data_matrix), -0.3, 0.7)))] <- NA
  colnames(data_matrix) <- c('rumination', 'happiness', 'activity')
  as.data.frame(data_matrix)
}


test_that('autovar function returns hello world', {
  with_mock(
    `parallel::clusterMap` = function(cluster, ...) {
      mapply(...)
    },
    `parallel::makeCluster` = function(...) {
      NULL
    },
    `parallel::stopCluster` = function(...) {
      NULL
    },
    expect_equal(class(autovar(testdata_raw_dataframe(),
                         selected_column_names = c('rumination',
                                                   'happiness',
                                                   'activity'),
                         imputation_iterations = 1)),
                 "list")
  )
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

