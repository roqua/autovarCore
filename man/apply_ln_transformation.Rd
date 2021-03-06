% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apply_ln_transformation.r
\name{apply_ln_transformation}
\alias{apply_ln_transformation}
\title{Applies the natural logarithm to the data set}
\usage{
apply_ln_transformation(data_matrix)
}
\arguments{
\item{data_matrix}{The original data matrix.}
}
\value{
The log-transformed data matrix.
}
\description{
This applies the ln function columnwise to the given input matrix and returns the modified matrix. If necessary, columns undergo a linear translation to ensure that all resulting values are >= 0.
}
\examples{
data_matrix <- matrix(1:10, dimnames = list(NULL, 'some_val'))
data_matrix
autovarCore:::apply_ln_transformation(data_matrix)
}
