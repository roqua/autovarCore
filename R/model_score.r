#' Return the model fit for the given varest model
#'
#' This function returns the model fit for the given model as either an AIC or BIC score. We compensating for logtransformation so that the model scores of logtransformed and non-logtransformed models can be compared with each other directly. This compensation is implemented by subtracting the logtransformed data from the log-likelihood score and using the result as log-likelihood score for the AIC/BIC calculations.
#' @param varest A \code{varest} model.
#' @param criterion A character string being either \code{'AIC'} or \code{'BIC'}.
#' @param use_logtransform A boolean, either \code{TRUE} or \code{FALSE}, indicating whether the input data for the model has been logtransformed.
#' @return This returns a floating point that is either the AIC or BIC criterion for the model. A lower number corresponds to a better model fit.
#' @examples
#' data_matrix <- matrix(nrow = 40, ncol = 3)
#' data_matrix[, ] <- runif(ncol(data_matrix) * nrow(data_matrix), 1, nrow(data_matrix))
#' colnames(data_matrix) <- c('rumination', 'happiness', 'activity')
#' varest <- autovarCore:::run_var(data_matrix, NULL, 1)
#' autovarCore:::model_score(varest, 'AIC', FALSE)
#' @export
model_score <- function(varest, criterion, use_logtransform) {
  criterion_function <- switch(criterion,
                               'AIC' = model_score_aic,
                               'BIC' = model_score_bic)
  criterion_function(varest, use_logtransform)
}

model_score_aic <- function(varest, use_logtransform) {

}

model_score_bic <- function(varest, use_logtransform) {

}
