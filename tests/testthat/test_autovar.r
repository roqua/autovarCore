context('autovar')

testdata_raw_dataframe <- function() {
  data_matrix <- matrix(nrow = 40, ncol = 3)
  data_matrix[, ] <- runif(ncol(data_matrix) * nrow(data_matrix), 1, nrow(data_matrix))
  while (sum(is.na(data_matrix)) == 0)
    data_matrix[as.logical(round(runif(ncol(data_matrix) * nrow(data_matrix), -0.3, 0.7)))] <- NA
  colnames(data_matrix) <- c('rumination', 'happiness', 'activity')
  as.data.frame(data_matrix)
}

testdata_data_matrix <- function() {
  structure(c(21.9545724191703, 34.6768329953775, 4.09459971333854,
              15.9624262561556, 15.290559650166, 29.5265263305046, 29.5241702131461,
              26.8065141087864, 6.01352940895595, 17.9962375783361, 12.0987575126346,
              19.2425501006655, 32.6173933204263, 8.90323126898147, 7.78844592696987,
              17.148062288994, 35.0033032100182, 39.7038455016445, 13.0827765115537,
              38.8779329438694, 28.3377237005625, 32.8060686755925, 11.2557470612228,
              33.9589908688795, 26.6941446098499, 3.36101600457914, 21.0341768339276,
              1.88202969916165, 32.4132719589397, 1.70697474596091, 4.69216786604375,
              2.63901332276873, 24.5904965591617, 29.0065981801599, 9.34198421938345,
              34.7525236639194, 9.49866789882071, 39.3655603309162, 9.25668670167215,
              28.55533367116, 33.5678325642366, 26.2055491164792, 17.9717030499596,
              27.5001922687516, 4.7937986899633, 9.29128465964459, 11.3919094381854,
              4.75331774377264, 24.4442545713391, 11.9371464657597, 12.9380554172676,
              38.0325647951104, 13.5485010412522, 7.37306111003272, 5.14180095493793,
              15.7762325457297, 18.4712027448695, 36.5357640166767, 37.0390428993851,
              21.3032927215099, 29.5595440363977, 5.92110942001455, 4.85455216304399,
              35.857443383662, 6.07369767781347, 4.69359725643881, 23.7706231633201,
              33.543985966593, 30.9382475479506, 21.6370199606754, 17.1177538551856,
              3.93860031431541, 4.8248987889383, 27.7620087808464, 6.84007635037415,
              37.1852702677716, 32.5731526366435, 8.94395732227713, 28.4558202228509,
              3.65072324080393, 7.27180969226174, 30.8623884017579, 32.09187052981,
              12.5787181677297, 19.8504055873491, 23.2913384437561, 4.24300696677528,
              5.98392160935327, 5.055167456856, 36.3529484947212, 38.3034561269451,
              20.7793815738987, 4.18002342130058, 26.7362216534093, 34.0865241554566,
              3.72119014873169, 14.8822493236512, 14.8610949178692, 31.4604263287038,
              38.7933044424281, 5.49562919232994, 22.1062562060542, 27.0718420641497,
              22.1150535834022, 18.3520701131783, 17.7398307933472, 20.6469970589969,
              17.8865942773409, 25.4992278048303, 26.7813279076945, 6.49851537263021,
              28.6000611640047, 1.66036160010844, 29.2091054741759, 26.7735339684878,
              37.4604364952538, 26.813571046805, 29.1080171857029, 4.64690295909531,
              15.0902988004964), .Dim = c(40L, 3L), .Dimnames = list(NULL,
                                                                     c("rumination",
                                                                       "happiness",
                                                                       "activity")))
}

testdata_residual_outliers <- function() {
  structure(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
            .Dim = c(40L, 3L), .Dimnames = list(NULL, c("rumination",
                                                        "happiness",
                                                        "activity")))
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

test_that('evaluate_model_config calls its subfunctions correctly without daydummies and trend', {
  lagg <<- 1
  data_matrix <<- testdata_data_matrix()
  res_outliers <<- testdata_residual_outliers()
  test_names <<- c('portmanteau', 'portmanteau_squared', 'skewness')
  criterion <<- 'AIC'
  logtransformed <<- FALSE
  significance_buckets <<- c(0.05, 0.01, 0.005, 0)
  called_count_needs_trend <<- 0
  called_count_daypart_dummies <<- 0
  called_count_residual_outliers <<- 0
  called_count_invalid_mask <<- 0
  called_count_select_valid <<- 0
  called_count_evaluate_model <<- 0
  called_count_compete <<- 0
  expected_result <-  list(model_score = 42,
                           lag = lagg,
                           bucket = 0.05,
                           nr_dummy_variables = 0)
  with_mock(
    `autovarCore:::needs_trend` = function(...) {
      called_count_needs_trend <<- called_count_needs_trend + 1
      expect_equal(list(...), list(data_matrix, lagg))
      FALSE
    },
    `autovarCore:::daypart_dummies` = function(...) {
      called_count_daypart_dummies <<- called_count_daypart_dummies + 1
      expect_equal(list(...), list(40, 1))
      NULL
    },
    `autovarCore:::residual_outliers` = function(...) {
      called_count_residual_outliers <<- called_count_residual_outliers + 1
      expect_equal(class(list(...)[[1]]), 'matrix')
      expect_equal(list(...)[[2]], 40)
      res_outliers
    },
    `autovarCore:::invalid_mask` = function(...) {
      called_count_invalid_mask <<- called_count_invalid_mask + 1
      expect_equal(list(...), list(res_outliers))
      0
    },
    `autovarCore:::select_valid_masks` = function(...) {
      called_count_select_valid <<- called_count_select_valid + 1
      expect_equal(list(...), list(0:7, 0))
      0:7
    },
    `autovarCore:::evaluate_model` = function(...) {
      called_count_evaluate_model <<- called_count_evaluate_model + 1
      expect_equal(list(...), list(called_count_evaluate_model - 1,
                                   data_matrix,
                                   NULL,
                                   lagg,
                                   res_outliers,
                                   test_names,
                                   criterion,
                                   logtransformed,
                                   significance_buckets))
      list(model_score = 50 - called_count_evaluate_model,
           lag = lagg,
           bucket = 0.05,
           nr_dummy_variables = 0)
    },
    `autovarCore:::compete` = function(...) {
      called_count_compete <<- called_count_compete + 1
      challenger_model <- list(model_score = 50 - called_count_compete,
                               lag = lagg,
                               bucket = 0.05,
                               nr_dummy_variables = 0)
      expect_equal(list(...)[[2]], challenger_model)
      challenger_model
    },
    expect_equal(autovarCore:::evaluate_model_config(0, data_matrix, test_names,
                                                     criterion, significance_buckets, 1),
                 expected_result)
  )
  expect_equal(called_count_needs_trend, 1)
  expect_equal(called_count_daypart_dummies, 1)
  expect_equal(called_count_residual_outliers, 1)
  expect_equal(called_count_invalid_mask, 1)
  expect_equal(called_count_select_valid, 1)
  expect_equal(called_count_evaluate_model, 8)
  expect_equal(called_count_compete, 8)
  rm(list = c('called_count_needs_trend',
              'called_count_daypart_dummies',
              'called_count_residual_outliers',
              'called_count_invalid_mask',
              'called_count_select_valid',
              'called_count_evaluate_model',
              'called_count_compete',
              'res_outliers',
              'data_matrix',
              'test_names',
              'criterion',
              'logtransformed',
              'significance_buckets',
              'lagg'), pos = '.GlobalEnv')
})

test_that('evaluate_model_config calls its subfunctions correctly with daydummies and trend', {
  lagg <<- 1
  data_matrix <<- testdata_data_matrix()
  res_outliers <<- testdata_residual_outliers()
  test_names <<- c('portmanteau', 'portmanteau_squared', 'skewness')
  criterion <<- 'AIC'
  logtransformed <<- FALSE
  significance_buckets <<- c(0.05, 0.01, 0.005, 0)
  called_count_needs_trend <<- 0
  called_count_daypart_dummies <<- 0
  called_count_residual_outliers <<- 0
  called_count_invalid_mask <<- 0
  called_count_select_valid <<- 0
  called_count_evaluate_model <<- 0
  called_count_compete <<- 0
  expected_result <-  list(model_score = 42,
                           lag = lagg,
                           bucket = 0.05,
                           nr_dummy_variables = 0)
  with_mock(
    `autovarCore:::needs_trend` = function(...) {
      called_count_needs_trend <<- called_count_needs_trend + 1
      expect_equal(list(...), list(data_matrix, lagg))
      FALSE
    },
    `autovarCore:::daypart_dummies` = function(...) {
      called_count_daypart_dummies <<- called_count_daypart_dummies + 1
      expect_equal(list(...), list(40, 1))
      NULL
    },
    `autovarCore:::residual_outliers` = function(...) {
      called_count_residual_outliers <<- called_count_residual_outliers + 1
      expect_equal(class(list(...)[[1]]), 'matrix')
      expect_equal(list(...)[[2]], 40)
      res_outliers
    },
    `autovarCore:::invalid_mask` = function(...) {
      called_count_invalid_mask <<- called_count_invalid_mask + 1
      expect_equal(list(...), list(res_outliers))
      0
    },
    `autovarCore:::select_valid_masks` = function(...) {
      called_count_select_valid <<- called_count_select_valid + 1
      expect_equal(list(...), list(0:7, 0))
      0:7
    },
    `autovarCore:::evaluate_model` = function(...) {
      called_count_evaluate_model <<- called_count_evaluate_model + 1
      expect_equal(list(...), list(called_count_evaluate_model - 1,
                                   data_matrix,
                                   NULL,
                                   lagg,
                                   res_outliers,
                                   test_names,
                                   criterion,
                                   logtransformed,
                                   significance_buckets))
      list(model_score = 50 - called_count_evaluate_model,
           lag = lagg,
           bucket = 0.05,
           nr_dummy_variables = 0)
    },
    `autovarCore:::compete` = function(...) {
      called_count_compete <<- called_count_compete + 1
      challenger_model <- list(model_score = 50 - called_count_compete,
                               lag = lagg,
                               bucket = 0.05,
                               nr_dummy_variables = 0)
      expect_equal(list(...)[[2]], challenger_model)
      challenger_model
    },
    expect_equal(autovarCore:::evaluate_model_config(0, data_matrix, test_names,
                                                     criterion, significance_buckets, 1),
                 expected_result)
  )
  expect_equal(called_count_needs_trend, 1)
  expect_equal(called_count_daypart_dummies, 1)
  expect_equal(called_count_residual_outliers, 1)
  expect_equal(called_count_invalid_mask, 1)
  expect_equal(called_count_select_valid, 1)
  expect_equal(called_count_evaluate_model, 8)
  expect_equal(called_count_compete, 8)
  rm(list = c('called_count_needs_trend',
              'called_count_daypart_dummies',
              'called_count_residual_outliers',
              'called_count_invalid_mask',
              'called_count_select_valid',
              'called_count_evaluate_model',
              'called_count_compete',
              'res_outliers',
              'data_matrix',
              'test_names',
              'criterion',
              'logtransformed',
              'significance_buckets',
              'lagg'), pos = '.GlobalEnv')
})

test_that('evaluate_model_config returns the correct result', {
  result <- autovarCore:::evaluate_model_config(1, testdata_data_matrix(),
                                                c('portmanteau', 'portmanteau_squared', 'skewness'),
                                                'AIC',
                                                c(0.05, 0.01, 0.005, 0),
                                                1)
  expect_equal(result$model_score, 936.893251367455)
  expect_equal(result$bucket, 0.005)
  expect_equal(result$nr_dummy_variables, 2)
  expect_equal(result$lag, 1)
  expect_equal(result$logtransformed, FALSE)
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


test_that('insert_model_into_list works when an empty list of models is given', {
  model <- list(bucket = 0.05, nr_dummy_variables = 1, model_score = 100)
  expected_result <- list(model)
  expect_equal(autovarCore:::insert_model_into_list(model, list(), TRUE),
               expected_result)
})

test_that('insert_model_into_list works when the model is to be appended to the end of the list', {
  model <- list(bucket = 0.05, nr_dummy_variables = 1, model_score = 100)
  model_list <- list(list(bucket = 0.05, nr_dummy_variables = 0, model_score = 100))
  expected_result <- append(model_list, list(model))
  expect_equal(autovarCore:::insert_model_into_list(model, model_list, TRUE),
               expected_result)
})

test_that('insert_model_into_list works when the model is to be prepended to the list', {
  model <- list(bucket = 0.05, nr_dummy_variables = 1, model_score = 100)
  model_list <- list(list(bucket = 0.05, nr_dummy_variables = 1, model_score = 101))
  expected_result <- append(model_list, list(model), after = 0)
  expect_equal(autovarCore:::insert_model_into_list(model, model_list, TRUE),
               expected_result)
})

test_that('insert_model_into_list inserts the model at the correct position', {
  model_list <- list(list(bucket = 0.05, nr_dummy_variables = 1, model_score = 103),
                     list(bucket = 0.05, nr_dummy_variables = 2, model_score = 101),
                     list(bucket = 0.01, nr_dummy_variables = 1, model_score = 99),
                     list(bucket = 0.01, nr_dummy_variables = 1, model_score = 101),
                     list(bucket = 0.01, nr_dummy_variables = 2, model_score = 101),
                     list(bucket = 0.005, nr_dummy_variables = 0, model_score = 101),
                     list(bucket = 0.005, nr_dummy_variables = 0, model_score = 102))
  model <- list(bucket = 0.01, nr_dummy_variables = 1, model_score = 100)
  expected_result <- append(model_list, list(model), after = 3)
  expect_equal(autovarCore:::insert_model_into_list(model, model_list, TRUE),
               expected_result)
  model <- list(bucket = 0.01, nr_dummy_variables = 0, model_score = 100)
  expected_result <- append(model_list, list(model), after = 2)
  expect_equal(autovarCore:::insert_model_into_list(model, model_list, TRUE),
               expected_result)
  model <- list(bucket = 0.05, nr_dummy_variables = 1, model_score = 105)
  expected_result <- append(model_list, list(model), after = 1)
  expect_equal(autovarCore:::insert_model_into_list(model, model_list, TRUE),
               expected_result)
})

test_that('insert_model_into_list handles the compare_outliers argument correctly', {
  model_list <- list(list(bucket = 0.05, nr_dummy_variables = 1, model_score = 103),
                     list(bucket = 0.05, nr_dummy_variables = 2, model_score = 101),
                     list(bucket = 0.01, nr_dummy_variables = 1, model_score = 99),
                     list(bucket = 0.01, nr_dummy_variables = 1, model_score = 101),
                     list(bucket = 0.01, nr_dummy_variables = 2, model_score = 101),
                     list(bucket = 0.005, nr_dummy_variables = 0, model_score = 101),
                     list(bucket = 0.005, nr_dummy_variables = 0, model_score = 102))
  model <- list(bucket = 0.01, nr_dummy_variables = 3, model_score = 100)
  expected_result <- append(model_list, list(model), after = 3)
  expect_equal(length(expected_result), length(model_list) + 1)
  expect_equal(autovarCore:::insert_model_into_list(model, model_list, FALSE),
               expected_result)
  model <- list(bucket = 0.01, nr_dummy_variables = 0, model_score = 100)
  expected_result <- append(model_list, list(model), after = 3)
  expect_equal(autovarCore:::insert_model_into_list(model, model_list, FALSE),
               expected_result)
  model <- list(bucket = 0.05, nr_dummy_variables = 1, model_score = 105)
  expected_result <- append(model_list, list(model), after = 2)
  expect_equal(autovarCore:::insert_model_into_list(model, model_list, FALSE),
               expected_result)
})


test_that('merge_model_lists works when one of the lists is empty', {
  model_list_a <- list(list(bucket = 0.05, nr_dummy_variables = 1, model_score = 103),
                       list(bucket = 0.05, nr_dummy_variables = 2, model_score = 101),
                       list(bucket = 0.01, nr_dummy_variables = 1, model_score = 99),
                       list(bucket = 0.01, nr_dummy_variables = 1, model_score = 101),
                       list(bucket = 0.01, nr_dummy_variables = 2, model_score = 101),
                       list(bucket = 0.005, nr_dummy_variables = 0, model_score = 101),
                       list(bucket = 0.005, nr_dummy_variables = 0, model_score = 102))
  model_list_b <- list()
  expect_equal(autovarCore:::merge_model_lists(model_list_a, model_list_b, TRUE),
               model_list_a)
  expect_equal(autovarCore:::merge_model_lists(model_list_b, model_list_a, TRUE),
               model_list_a)
})

test_that('merge_model_lists works when one of the lists should be place fully behind the other', {
  model_list_a <- list(list(bucket = 0.05, nr_dummy_variables = 1, model_score = 103),
                       list(bucket = 0.05, nr_dummy_variables = 2, model_score = 101),
                       list(bucket = 0.01, nr_dummy_variables = 1, model_score = 99),
                       list(bucket = 0.01, nr_dummy_variables = 1, model_score = 101),
                       list(bucket = 0.01, nr_dummy_variables = 2, model_score = 101),
                       list(bucket = 0.005, nr_dummy_variables = 0, model_score = 101),
                       list(bucket = 0.005, nr_dummy_variables = 0, model_score = 102))
  model_list_b <- list(list(bucket = 0.005, nr_dummy_variables = 0, model_score = 103),
                       list(bucket = 0.005, nr_dummy_variables = 1, model_score = 101),
                       list(bucket = 0.005, nr_dummy_variables = 2, model_score = 99),
                       list(bucket = 0.005, nr_dummy_variables = 2, model_score = 101),
                       list(bucket = 0.001, nr_dummy_variables = 0, model_score = 100),
                       list(bucket = 0.001, nr_dummy_variables = 0, model_score = 101),
                       list(bucket = 0.001, nr_dummy_variables = 3, model_score = 102))
  expected_result <- append(model_list_a, model_list_b)
  expect_equal(length(expected_result), length(model_list_a) + length(model_list_b))
  expect_equal(autovarCore:::merge_model_lists(model_list_a, model_list_b, TRUE),
               expected_result)
  expect_equal(autovarCore:::merge_model_lists(model_list_b, model_list_a, TRUE),
               expected_result)
})

test_that('merge_model_lists works when lists need to be interleaved', {
  model_list_a <- list(list(bucket = 0.05, nr_dummy_variables = 1, model_score = 103),
                       list(bucket = 0.01, nr_dummy_variables = 1, model_score = 99),
                       list(bucket = 0.01, nr_dummy_variables = 2, model_score = 101),
                       list(bucket = 0.005, nr_dummy_variables = 0, model_score = 102),
                       list(bucket = 0.005, nr_dummy_variables = 1, model_score = 101),
                       list(bucket = 0.005, nr_dummy_variables = 2, model_score = 101),
                       list(bucket = 0.001, nr_dummy_variables = 0, model_score = 101))
  model_list_b <- list(list(bucket = 0.05, nr_dummy_variables = 2, model_score = 101),
                       list(bucket = 0.01, nr_dummy_variables = 1, model_score = 101),
                       list(bucket = 0.005, nr_dummy_variables = 0, model_score = 101),
                       list(bucket = 0.005, nr_dummy_variables = 0, model_score = 103),
                       list(bucket = 0.005, nr_dummy_variables = 2, model_score = 99),
                       list(bucket = 0.001, nr_dummy_variables = 0, model_score = 100),
                       list(bucket = 0.001, nr_dummy_variables = 3, model_score = 102))
  expected_result <- list(list(bucket = 0.05, nr_dummy_variables = 1, model_score = 103),
                          list(bucket = 0.05, nr_dummy_variables = 2, model_score = 101),
                          list(bucket = 0.01, nr_dummy_variables = 1, model_score = 99),
                          list(bucket = 0.01, nr_dummy_variables = 1, model_score = 101),
                          list(bucket = 0.01, nr_dummy_variables = 2, model_score = 101),
                          list(bucket = 0.005, nr_dummy_variables = 0, model_score = 101),
                          list(bucket = 0.005, nr_dummy_variables = 0, model_score = 102),
                          list(bucket = 0.005, nr_dummy_variables = 0, model_score = 103),
                          list(bucket = 0.005, nr_dummy_variables = 1, model_score = 101),
                          list(bucket = 0.005, nr_dummy_variables = 2, model_score = 99),
                          list(bucket = 0.005, nr_dummy_variables = 2, model_score = 101),
                          list(bucket = 0.001, nr_dummy_variables = 0, model_score = 100),
                          list(bucket = 0.001, nr_dummy_variables = 0, model_score = 101),
                          list(bucket = 0.001, nr_dummy_variables = 3, model_score = 102))
  expect_equal(length(expected_result), length(model_list_a) + length(model_list_b))
  expect_equal(autovarCore:::merge_model_lists(model_list_a, model_list_b, TRUE),
               expected_result)
  expect_equal(autovarCore:::merge_model_lists(model_list_b, model_list_a, TRUE),
               expected_result)
})
