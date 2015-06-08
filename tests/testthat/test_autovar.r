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
                         list(selected_column_names = c('rumination',
                                                        'happiness',
                                                        'activity'),
                              imputation_iterations = 1))),
                 "list")
  )
})
