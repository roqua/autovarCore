#' Tests the white noise assumption for a VAR model using a portmanteau test on the residuals
#'
#' This function tests the white noise assumption for the residuals of the endogenous variables in the specified VAR model. This function implements the portmanteau test known as the Ljung-Box test, and results are comparable with STATA's \code{wntestq}. Of the p-levels resulting from assessing the white noise assumption for the residuals of that variable, the minimum is returned.
#' @param varest A \code{varest} model.
#' @return This function returns a p-level.
#' @examples
#' data_matrix <- matrix(nrow = 40, ncol = 3)
#' data_matrix[, ] <- runif(ncol(data_matrix) * nrow(data_matrix), 1, nrow(data_matrix))
#' colnames(data_matrix) <- c('rumination', 'happiness', 'activity')
#' varest <- autovarCore:::run_var(data_matrix, NULL, 1)
#' autovarCore:::assess_portmanteau(varest)
#' @importFrom stats resid
#' @importFrom stats pchisq
assess_portmanteau <- function(varest) {
  data <- unname(resid(varest))
  portmanteau_test_data(data)
}

portmanteau_test_data <- function(data) {
  # This function is also used by assess_portmanteau_squared.
  nr_cols <- ncol(data)
  nr_rows <- nrow(data)
  if (is.null(nr_cols) || nr_cols < 1 || is.null(nr_rows) || nr_rows < 1)
    stop("No residuals found")
  port_lags <- determine_portmanteau_lags(data)
  if (port_lags < 1)
    stop("Not enough observations in the data")
  minimum_p_level_port <- Inf
  port_test_statistics <- portmanteau_test_statistics(data)
  for (column_index in 1:nr_cols) {
    port_test_statistic <- port_test_statistics[column_index]
    p_level_port <- chi_squared_prob(port_test_statistic, port_lags)
    if (p_level_port < minimum_p_level_port)
      minimum_p_level_port <- p_level_port
  }
  minimum_p_level_port
}

determine_portmanteau_lags <- function(data) {
  # This is the default value used in STATA.
  min(floor(nrow(data)/2) - 2, 40)
}

chi_squared_prob <- function(q, h) {
  pchisq(q, h, lower.tail = FALSE)
}
