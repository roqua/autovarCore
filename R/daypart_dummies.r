#' Calculate day-part dummy variables
#'
#' This function returns either \code{NULL} (if \code{measurements_per_day} is 0 or 1) or a matrix of dummy variables for the specified input configuration.
#' @param number_of_rows the number of rows in the input data set.
#' @param measurements_per_day the number of measurements per day in the input data set.
#' @return Either \code{NULL} or a matrix with \code{number_of_rows} rows and \code{measurements_per_day - 1} columns.
#' @examples
#' autovarCore:::daypart_dummies(10, 3)
#' @export
daypart_dummies <- function(number_of_rows, measurements_per_day) {
  if (measurements_per_day == 0 || measurements_per_day == 1)
    return(NULL)
  result <- NULL
  for (column_index in 1:(measurements_per_day - 1))
    result <- cbind(result, seasonal_dummy_column(number_of_rows,
                                                  measurements_per_day,
                                                  column_index - 1))
  colnames(result) <- dummy_column_names(ncol(result))
  result
}

seasonal_dummy_column <- function(out_length, period, offset) {
  result <- c(rep.int(0, times = offset),
              1,
              rep.int(0, times = period - offset - 1))
  rep.int(result,
          times = ceiling(out_length / period))[1:out_length]
}

dummy_column_names <- function(number_of_columns) {
  result <- NULL
  for (i in 1:number_of_columns)
    result <- c(result, paste('dailymeas_', i, sep = ''))
  result
}
