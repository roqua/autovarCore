#' Eigenvalue stability condition checking
#'
#' This function returns whether the given model satisfies the Eigenvalue stability condition. The Eigenvalue stability condition is satisfied when all eigen values lie in the unit circle.
#' @param varest A \code{varest} model.
#' @return This function returns \code{TRUE} if the model satisfies the Eigenvalue stability condition and \code{FALSE} otherwise.
#' @examples
#' data_matrix <- matrix(nrow = 40, ncol = 3)
#' data_matrix[, ] <- runif(ncol(data_matrix) * nrow(data_matrix), 1, nrow(data_matrix))
#' while (sum(is.na(data_matrix)) == 0)
#'   data_matrix[as.logical(round(runif(ncol(data_matrix) * nrow(data_matrix), -0.3, 0.7)))] <- NA
#' colnames(data_matrix) <- c('rumination', 'happiness', 'activity')
#' varest <- autovarCore:::run_var(data_matrix, NULL, 1)
#' autovarCore:::model_is_stable(varest)
#' @export
model_is_stable <- function(varest) {
  modulus_eigen_values <- roots(varest, modulus = TRUE)
  all(modulus_eigen_values < 1)
}
