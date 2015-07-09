#include <Rcpp.h>
using namespace Rcpp;


//' Skewness coefficients.
//'
//' @param matrix the matrix of residuals.
//' @export
// [[Rcpp::export]]
NumericVector coefficients_of_skewness(NumericMatrix matrix) {
  int nrows = matrix.nrow(), ncols = matrix.ncol();
  NumericVector result(ncols);
  double denom = 1.0/nrows;
  for (int col = 0; col < ncols; col++) {
    double mu = 0;
    for (int row = 0; row < nrows; row++)
      mu += matrix(row, col);
    mu /= nrows;
    double m3 = 0, m2 = 0;
    for (int row = 0; row < nrows; row++) {
      m3 += pow(matrix(row, col) - mu, 3);
      m2 += pow(matrix(row, col) - mu, 2);
    }
    m2 *= denom;
    m3 *= denom;
    result[col] = m3 * pow(m2, -1.5);
  }
  return result;
}
