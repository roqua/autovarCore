// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// coefficients_of_kurtosis
NumericVector coefficients_of_kurtosis(NumericMatrix matrix);
RcppExport SEXP _autovarCore_coefficients_of_kurtosis(SEXP matrixSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type matrix(matrixSEXP);
    rcpp_result_gen = Rcpp::wrap(coefficients_of_kurtosis(matrix));
    return rcpp_result_gen;
END_RCPP
}
// coefficients_of_skewness
NumericVector coefficients_of_skewness(NumericMatrix matrix);
RcppExport SEXP _autovarCore_coefficients_of_skewness(SEXP matrixSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type matrix(matrixSEXP);
    rcpp_result_gen = Rcpp::wrap(coefficients_of_skewness(matrix));
    return rcpp_result_gen;
END_RCPP
}
// portmanteau_test_statistics
NumericVector portmanteau_test_statistics(NumericMatrix matrix);
RcppExport SEXP _autovarCore_portmanteau_test_statistics(SEXP matrixSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type matrix(matrixSEXP);
    rcpp_result_gen = Rcpp::wrap(portmanteau_test_statistics(matrix));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_autovarCore_coefficients_of_kurtosis", (DL_FUNC) &_autovarCore_coefficients_of_kurtosis, 1},
    {"_autovarCore_coefficients_of_skewness", (DL_FUNC) &_autovarCore_coefficients_of_skewness, 1},
    {"_autovarCore_portmanteau_test_statistics", (DL_FUNC) &_autovarCore_portmanteau_test_statistics, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_autovarCore(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
