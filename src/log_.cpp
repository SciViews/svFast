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

struct Logw : public Worker {
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
//' @param x vector of numeric values
//' @param base the base of the logarithm (e = `exp(1)` by default)
//' @param paralen the minimum length of `x` to use parallel computation (50000
//' by default)
//' 
//' @return A numeric vector, matrix, or data frame with the transformed values.
//' 
//' @details
//' This function does not behave exactly as [base::log()]. First, it
//' delegates to [base::log()] when x is a **factor**, a **Date**,
//' a **POSIXt**, a **difftime**, a **complex** vector or an **S4** object.
//' 
//' Second, for other objects than **S4**, it just computes the log and returns
//' a similar object with identical attributes. If you need a method for, say,
//' an S3 or S7 object, write it for **Math** and use [log_()] inside it for
//' faster computation of large vectors. For **data.frame** it performs the
//' computation on each column.
//' 
//' Finally, it does not warn in case `NaN` is returned somewhere in the vector
//' ([base::log()] does).
//' 
//' @export
//' @examples
//' log_(1:5)
//' log_(1:5, base = 2.5)
// [[Rcpp::export]]
RObject log_(RObject x, double base = 2.718282, const R_xlen_t paralen = 5e4) {
  // fix base (if default value is provided)
  if (base == 2.718282) // How could I default to exp(1) otherwise ?
    base = M_E;
  const double logbase = log(base);

  // Check x
  if (Rf_isFactor(x) || is<Date>(x) || x.inherits("POSIXt") ||
    x.inherits("difftime") || is<ComplexVector>(x) || x.isS4()) {
    // delegate to base R log()
    Function f("log");
    return f(x, _["base"] = base);
  
  } else if (is<DataFrame>(x)) {
    // Apply the log_ function to each column of the data frame
    // (there may be more efficient ways to do this, but this is simple)
    DataFrame df = clone(as<DataFrame>(x));
    for (R_xlen_t i = 0; i < df.cols(); ++i) {
      df[i] = log_(df[i], base, paralen);
    }
    // TODO: Special treatment for data.tables here?
    // because data.table:::selfrefok(df) is FALSE now -> not good
    return df;
  
  } else if (!is<NumericVector>(x) && !is<IntegerVector>(x) && !is<LogicalVector>(x)) {
    stop("Non-numeric argument to mathematical function");
  }
  
  NumericVector xnum = as<NumericVector>(x);
  R_xlen_t n = xnum.length();
  NumericVector output = no_init(n);
    
  if (n < paralen) {
    for (R_xlen_t i = 0; i < n; ++i) {
      output[i] = log(xnum[i]) / logbase;
    }
      
  } else {// Parallel computation
    Logw logw(xnum, logbase, output);
    parallelFor(0, n, logw);
  }
  
  // Copy attributes from x to output here
  DUPLICATE_ATTRIB(output, x);
  // TODO: Special treatment for label and units ?
  
  return output;
}

// Note: this was a simpler and faster version with n = 1e9, but it accepts
// only NumericVector and does not deal with data frames or attributes.
// NumericVector log_(NumericVector x, double base = 2.718282,
//   const R_xlen_t paralen = 50000) {
//   if (base == 2.718282) // How could I default to exp(1) otherwise ?
//     base = M_E;
//   R_xlen_t n = x.length();
//   NumericVector output = no_init(n);
//   const double logbase = log(base);
//   
//   if (x.length() < paralen) {
//     for (R_xlen_t i = 0; i < n; ++i) {
//       output[i] = log(x[i]) / logbase;
//     }
//     
//   } else {// Parallel computation
//     Logw logw(x, logbase, output);
//     parallelFor(0, x.length(), logw);
//   }
//   
//   return output;
// }

