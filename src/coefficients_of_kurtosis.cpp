#include <Rcpp.h>
using namespace Rcpp;


//' Kurtosis coefficients.
//'
//' @param matrix the matrix of residuals.
// [[Rcpp::export]]
NumericVector coefficients_of_kurtosis(NumericMatrix matrix) {
  int nrows = matrix.nrow(), ncols = matrix.ncol();
  NumericVector result(ncols);
  double denom = 1.0/nrows;
  for (int col = 0; col < ncols; col++) {
    double mu = 0;
    for (int row = 0; row < nrows; row++)
      mu += matrix(row, col);
    mu /= nrows;
    double m4 = 0, m2 = 0;
    for (int row = 0; row < nrows; row++) {
      m4 += pow(matrix(row, col) - mu, 4);
      m2 += pow(matrix(row, col) - mu, 2);
    }
    m2 *= denom;
    m4 *= denom;
    result[col] = m4 * pow(m2, -2);
  }
  return result;
}
