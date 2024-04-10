/**
 * A parallel version of various math functions.
 */

// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>
#include <cmath>
#include <algorithm>

using namespace Rcpp;
using namespace RcppParallel;
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

// The pointer to the function to use
double (*mathfun)(double);

struct Cosw : public Worker
{   
  // source vector
  const RVector<double> input;
  
  // result vector
  RVector<double> output;
  
  // initialize with source and destination
  Cosw(const NumericVector input, NumericVector output) 
    : input(input), output(output) {}
  
  // get the cosine of the element of the range I've been asked to
  void operator()(std::size_t begin, std::size_t end) {
    std::transform(input.begin() + begin, input.begin() + end, 
      output.begin() + begin, ::mathfun);
  }
};

NumericVector do_math(NumericVector x, double (*mathfun)(double)) {
  // allocate the output vector
  R_xlen_t n = x.length();
  NumericVector output = no_init(n);
  
  // should it be run sequentially?
  if (x.length() < 50000) {
    for (R_xlen_t i = 0; i < n; ++i) {
      output[i] = mathfun(x[i]);
    }
    // direct Rcpp call (but same speed)
    //NumericVector output = cos(x);
    
  } else {// Parallel computation
    // Cosw functor (pass input and output vectors)
    Cosw cosw(x, output);
    
    // call parallelFor to do the work
    parallelFor(0, x.length(), cosw);
  }
  
  return output;
}

//' Fast parallel version of cosinus (when vector size >= 50000)
//'
//' @param x vector a numeric values
//' @param paralen the minimum length of x to use parallel computation (50000
//' by default)
//' @return a numeric vector with the cos(x) values
//' @export
//' @example
//' cos_(1:5)
// [[Rcpp::export]]
 NumericVector cos_(NumericVector x, const R_xlen_t paralen = 50000) {
   mathfun = &std::cos;
   
   R_xlen_t n = x.length();
   NumericVector output = no_init(n);
   
   if (n < paralen) {
     for (R_xlen_t i = 0; i < n; ++i) {
       output[i] = mathfun(x[i]);
     }
     
   } else {// Parallel computation
     Cosw cosw(x, output);
     parallelFor(0, n, cosw);
   }
   
   return output;
 }
 