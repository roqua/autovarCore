\name{autovarCore-package}
\alias{autovarCore-package}
\alias{autovarCore}
\docType{package}
\title{
\packageTitle{autovarCore}
}
\description{
\packageDescription{autovarCore}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{autovarCore}
\packageIndices{autovarCore}
Please see the help of the \code{\link{autovar}} function for information on how to use this package.
}
\author{
\packageAuthor{autovarCore}

Maintainer: \packageMaintainer{autovarCore}
}
\references{
Emerencia, A. C., L. van der Krieke, E. H. Bos, P. de Jonge, N. Petkov and M. Aiello (2016), Automating Vector Autoregression on Electronic Patient Diary Data, \emph{IEEE Journal of Biomedical and Health Informatics}, \bold{20(2)}: 631-643, \url{https://doi.org/10.1109/JBHI.2015.2402280}
}
\keyword{ regression }
\keyword{ ts }
\keyword{ multivariate }
\keyword{ package }
\concept{ VAR }
\concept{ vector autoregression }
\concept{ timeseries }
\concept{ contemporaneous correlation }
\concept{ granger causality }
\seealso{
  \code{\link{autovar}}
}
\examples{
\dontrun{
# AutovarCore requires input data in data.frame format.
# If you have data in a .csv, .dta, or .sav file, use
# the 'foreign' library to load this data into R first.
# (You may need to type:
#    install.packages('foreign')
#  if you do not have the foreign library installed on
#  your system.)

# This example data set can be downloaded from
# https://autovar.nl/datasets/aug_pp5_da.sav
suppressWarnings(dfile <- foreign::read.spss('~/Downloads/aug_pp5_da.sav'))
dframe <- data.frame(Activity = dfile$Activity, Depression = dfile$Depression)

# Call autovar with the given data frame. Type:
#   ?autovar
# (after having typed "library('autovarCore')") to see
# which other options are available.
models_found <- autovar(dframe, selected_column_names = c('Activity', 'Depression'))

# Show details for the best model found
print(models_found[[1]])
}
}
