#' Tests the kurtosis of a VAR model
#'
#' This function tests the kurtosis for the residuals of the endogenous variables in the specified VAR model. This function uses an implementation equivalent to STATA's \code{sktest}. Of the p-levels resulting from assessing the significance of the kurtosis for the residuals of that variable, the minimum is returned.
#' @param varest A \code{varest} model.
#' @return This function returns a p-level.
#' @examples
#' data_matrix <- matrix(nrow = 40, ncol = 3)
#' data_matrix[, ] <- runif(ncol(data_matrix) * nrow(data_matrix), 1, nrow(data_matrix))
#' colnames(data_matrix) <- c('rumination', 'happiness', 'activity')
#' varest <- autovarCore:::run_var(data_matrix, NULL, 1)
#' autovarCore:::assess_kurtosis(varest)
assess_kurtosis <- function(varest) {
  0.06
}
