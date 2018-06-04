#' Print the correlation matrix of the residuals of a model annotated with p-values
#'
#' This function prints the correlation matrix of residuals of a model annotated with p-values. This is a lower triangular matrix, in the way that all elements in the upper triangular matrix are \code{NA} and the elementals on the "diagonal" are \code{1} (note that there is not really a diagonal because the matrix is rectangular). The odd rows of the returned matrix contain the correlations while the even rows are the associated p-values. For each correlation in row \code{x}, column \code{y}, its p-value is located in row \code{x+1}, column {y}.
#' @param varest A \code{varest} model.
#' @return This function returns the annotated correlation matrix.
#' @examples
#' data_matrix <- matrix(nrow = 40, ncol = 3)
#' data_matrix[, ] <- runif(ncol(data_matrix) * nrow(data_matrix), 1, nrow(data_matrix))
#' colnames(data_matrix) <- c('rumination', 'happiness', 'activity')
#' varest <- autovarCore:::run_var(data_matrix, NULL, 1)
#' autovarCore::print_correlation_matrix(varest)
#' @export
print_correlation_matrix <- function(varest) {
  aug_correlation_matrix <- augmented_correlation_matrix(summary(varest))
  cat("\nCorrelation matrix of residuals:\n")
  print(aug_correlation_matrix, digits = 8)
  invisible(aug_correlation_matrix)
}

augmented_correlation_matrix <- function(varsum) {
  correlation_matrix <- varsum$corres
  nresids <- length(varsum$varresult[[1]]$residuals)
  if (nresids == 0 || is.null(correlation_matrix) || any(dim(correlation_matrix) == 0))
    return(correlation_matrix)
  nr_rows <- dim(correlation_matrix)[[1]]
  nr_cols <- dim(correlation_matrix)[[2]]
  aug_dimnames <- NULL
  for (i in 1:(length(dimnames(correlation_matrix)[[1]])))
    aug_dimnames <- c(aug_dimnames, dimnames(correlation_matrix)[[1]][[i]], "p")
  aug_correlation_matrix <- matrix(NA, nrow = 2 * nr_rows, ncol = nr_cols,
                                   dimnames = list(aug_dimnames,
                                                   dimnames(correlation_matrix)[[2]]))
  for (i in 1:nr_rows)
    for (j in 1:nr_cols)
      if (i >= j) {
        aug_correlation_matrix[2 * i - 1, j] <- correlation_matrix[i, j]
        aug_correlation_matrix[2 * i, j] <- significance_from_pearson_coef(correlation_matrix[i, j],
                                                                           nresids)
      }
  aug_correlation_matrix
}

#' Calculate the significance of a Pearson correlation
#'
#' @importFrom stats pt
significance_from_pearson_coef <- function(p, n) {
  2 * pt(abs(p) * sqrt(n - 2) / sqrt(1 - (p * p)), n - 2, lower.tail = FALSE)
}
