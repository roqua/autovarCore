context('assess_portmanteau')

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

test_that('assess_portmanteau returns the correct result', {
  varest <- autovarCore:::run_var(testdata_data_matrix(), NULL, 1)
  expect_lt(abs(autovarCore:::assess_portmanteau(varest) - 0.006923178), 0.0000001)
})

test_that('assess_portmanteau calls its subfunctions', {
  varest <<- autovarCore:::run_var(testdata_data_matrix(), NULL, 1)
  called_count <<- 0
  with_mock(
    `autovarCore:::portmanteau_test_data` = function(...) {
      called_count <<- called_count + 1
      expect_equal(list(...), list(unname(resid(varest))))
      1
    },
    expect_equal(autovarCore:::assess_portmanteau(varest), 1)
  )
  expect_equal(called_count, 1)
  rm(list = c('called_count', 'varest'), pos = '.GlobalEnv')
})


test_that('portmanteau_test_data calls its subfunctions', {
  called_count_statistic <<- 0
  called_count_chi_squared <<- 0
  called_count_pt_lags <<- 0
  varest <<- autovarCore:::run_var(testdata_data_matrix(), NULL, 1)
  portmanteau_statistics <<- c(26, 27, 28)
  chi_squared_probs <<- c(0.7, 0.6, 0.8)
  with_mock(
    `autovarCore:::determine_portmanteau_lags` = function(...) {
      called_count_pt_lags <<- called_count_pt_lags + 1
      expect_equal(list(...), list(unname(resid(varest))))
      17
    },
    `autovarCore:::portmanteau_test_statistics` = function(...) {
      called_count_statistic <<- called_count_statistic + 1
      expect_equal(list(...), list(unname(resid(varest))))
      portmanteau_statistics
    },
    `autovarCore:::chi_squared_prob` = function(...) {
      called_count_chi_squared <<- called_count_chi_squared + 1
      expect_equal(list(...), list(portmanteau_statistics[called_count_chi_squared], 17))
      chi_squared_probs[called_count_chi_squared]
    },
    expect_lt(abs(autovarCore:::assess_portmanteau(varest) - 0.6), 0.0000001)
  )
  expect_equal(called_count_statistic, 1)
  expect_equal(called_count_chi_squared, 3)
  expect_equal(called_count_pt_lags, 1)
  rm(list = c('called_count_statistic',
              'called_count_chi_squared',
              'called_count_pt_lags',
              'portmanteau_statistics', 'chi_squared_probs', 'varest'), pos = '.GlobalEnv')
})
