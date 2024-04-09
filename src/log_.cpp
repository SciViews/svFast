/**
 * A parallel version of log(x).
 */

// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>
#include <cmath>
#include <algorithm>

using namespace Rcpp;
using namespace RcppParallel;

struct Logf {
  double logbase;
public:  
  Logf(double logbase = 1) : logbase(logbase) {}
  double operator()(double x) { return log(x) / logbase; }
};

struct Logw : public Worker
{
  const RVector<double> input;
  const double logbase;
  RVector<double> output;
public:
  Logw(const NumericVector input, const double logbase, NumericVector output) 
    : input(input), logbase(logbase), output(output) {}
  void operator()(std::size_t begin, std::size_t end) {
    std::transform(input.begin() + begin, input.begin() + end, 
      output.begin() + begin, Logf(logbase));
  }
};

//' Fast parallel version of log(x, base) (when vector size >= 50000)
//'
//' @param x vector a numeric values
//' @param base the base of the logarithm (e = exp(1) by default)
//' @param paralen the minimum length of x to use parallel computation (50000
//' by default)
//' @return a numeric vector with the log_base(x) values
//' @export
//' @example
//' log_(1:5)
//' log_(1:5, base = 2.5)
// [[Rcpp::export]]
 NumericVector log_(NumericVector x, double base = 2.718282,
     const R_xlen_t paralen = 50000) {
   if (base == 2.718282) // How could I default to exp(1) otherwise ?
      base = M_E;
   R_xlen_t n = x.length();
   NumericVector output = no_init(n);
   const double logbase = log(base);
   
   if (x.length() < paralen) {
     for (R_xlen_t i = 0; i < n; ++i) {
       output[i] = log(x[i]) / logbase;
     }
     
   } else {// Parallel computation
     Logw logw(x, logbase, output);
     parallelFor(0, x.length(), logw);
   }
   
   return output;
 }
 