#' Tests the skewness of a VAR model
#'
#' This function tests the skewness for the residuals of the endogenous variables in the specified VAR model. This function uses an implementation equivalent to STATA's \code{sktest}. Of the p-levels resulting from assessing the significance of the skewness for the residuals of that variable, the minimum is returned.
#' @param varest A \code{varest} model.
#' @return This function returns a p-level.
#' @examples
#' data_matrix <- matrix(nrow = 40, ncol = 3)
#' data_matrix[, ] <- runif(ncol(data_matrix) * nrow(data_matrix), 1, nrow(data_matrix))
#' colnames(data_matrix) <- c('rumination', 'happiness', 'activity')
#' varest <- autovarCore:::run_var(data_matrix, NULL, 1)
#' autovarCore:::assess_skewness(varest)
#' @importFrom stats resid
#' @importFrom stats pnorm
assess_skewness <- function(varest) {
  resids <- unname(resid(varest))
  nr_cols <- ncol(resids)
  nr_rows <- nrow(resids)
  if (is.null(nr_cols) || nr_cols < 1 || is.null(nr_rows) || nr_rows < 1)
    stop("No residuals found")
  minimum_p_level_skew <- Inf
  coefficients_of_skew <- coefficients_of_skewness(resids)
  for (column_index in 1:nr_cols) {
    coef_of_skewness <- coefficients_of_skew[column_index]
    z_skew <- z_skewness(coef_of_skewness, nr_rows)
    p_level_skew <- 2 - 2 * pnorm(abs(z_skew))
    if (p_level_skew < minimum_p_level_skew)
      minimum_p_level_skew <- p_level_skew
  }
  minimum_p_level_skew
}

z_skewness <- function(g1, n) {
  # This function is also used by assess_joint_sktest.
  Y <- g1 * sqrt((n + 1) * (n + 3)/(6 * (n - 2)))
  B2g1 <- (3 * (n^2 + 27 * n -70) * (n + 1) * (n + 3))/((n - 2) * (n + 5) * (n + 7) * (n + 9))
  W2 <- -1 + sqrt(2 * (B2g1 - 1))
  a <- sqrt(2/(W2 - 1))
  Z1 <- (1/(sqrt(log(sqrt(W2))))) * log((Y/a) + sqrt((Y/a)^2 + 1))
  Z1
}
