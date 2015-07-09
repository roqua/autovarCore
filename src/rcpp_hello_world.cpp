#include <Rcpp.h>
using namespace Rcpp;

//' Simple function using Rcpp
//'
//' @export
// [[Rcpp::export]]
List rcpp_hello_world() {

    CharacterVector x = CharacterVector::create( "foo", "bar" )  ;
    NumericVector y   = NumericVector::create( 0.0, 1.0 ) ;
    List z            = List::create( x, y ) ;

    return z ;
}


//' A faster version of portmanteau_test_statistic.
//'
//' @param matrix the matrix of residuals or squared residuals.
//' @export
// [[Rcpp::export]]
NumericVector portmanteau_test_statistics(NumericMatrix matrix) {
  int nrows = matrix.nrow(), ncols = matrix.ncol();
  int port_lags = (nrows/2) - 2; // refactor this later
  if (port_lags > 40)
    port_lags = 40;
  NumericVector result(ncols);
  for (int col = 0; col < ncols; col++) {
    double mu = 0;
    for (int row = 0; row < nrows; row++)
      mu += matrix(row, col);
    mu /= nrows;
    for (int row = 0; row < nrows; row++)
      matrix(row,col) -= mu;
    double suma = 0, denom = 0;
    for (int row = 0; row < nrows; row++) {
      double val = matrix(row, col);
      denom += val * val;
    }
    for (int k = 1; k <= port_lags; k++) {
      double sample_autocorrelation = 0;
      for (int row = k + 1; row <= nrows; row++)
        sample_autocorrelation += matrix(row - 1, col) * matrix(row - k - 1, col);
      sample_autocorrelation /= denom;
      suma += (sample_autocorrelation * sample_autocorrelation)/(nrows - k);
    }
    suma *= nrows * (nrows + 2);
    result[col] = suma;
  }
  return result;
}
