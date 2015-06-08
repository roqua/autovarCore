#' Returns the winning model
#'
#' This function returns the best model as explained in the documentation for the \code{autovar} function.
#' @param best A model given as a list with at least the properties \code{varest, model_score,} and \code{bucket}.
#' @param challenger Another model, also given as a list with properties \code{varest, model_score,} and \code{bucket}.
#' @param compare_outliers A boolean. When \code{FALSE}, the model comparison does not take the number of dummy variables into account.
#' @return This function returns the best model of the two given models.
#' @examples
#' data_matrix <- matrix(nrow = 40, ncol = 3)
#' data_matrix[, ] <- runif(ncol(data_matrix) * nrow(data_matrix), 1, nrow(data_matrix))
#' colnames(data_matrix) <- c('rumination', 'happiness', 'activity')
#' varest1 <- autovarCore:::run_var(data_matrix, NULL, 1)
#' model1 <- list(logtransformed = FALSE, lag = 1, varest = varest1, model_score = 100, bucket = 0.05)
#' varest2 <- autovarCore:::run_var(data_matrix, NULL, 2)
#' model2 <- list(logtransformed = FALSE, lag = 2, varest = varest2, model_score = 200, bucket = 0.01)
#' autovarCore:::compete(model1, model2, TRUE)
#' @export
compete <- function(best, challenger, compare_outliers) {
  if (challenger$bucket != best$bucket) {
    if (challenger$bucket > best$bucket)
      return(challenger)
    return(best)
  }
  if (compare_outliers) {
    nr_dummy_variables_best = nr_dummy_variables(best$varest)
    nr_dummy_variables_challenger = nr_dummy_variables(challenger$varest)
    if (nr_dummy_variables_challenger < nr_dummy_variables_best) {
      return(challenger)
    } else if (nr_dummy_variables_best < nr_dummy_variables_challenger) {
      return(best)
    }
  }
  if (challenger$model_score < best$model_score)
    return(challenger)
  return(best)
}

nr_dummy_variables <- function(varest) {
  outlier_dummies <- length(grep("^outlier_[0-9]+$", colnames(varest$datamat)))
  day_dummies <- length(grep("^day_[0-9]+$", colnames(varest$datamat)))
  if (day_dummies > 0)
    day_dummies <- 1
  outlier_dummies + day_dummies
}
