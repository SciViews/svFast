/**
 * A parallel version of log(x).
 */

// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>
#include <cmath>
#include <algorithm>
#include <Rmath.h>

using namespace Rcpp;
using namespace RcppParallel;

// We use polymorphic functors passed to std::transform() in Mathw
// https://www.fluentcpp.com/2018/04/17/pass-polymorphic-object-stl-algorithm/
struct Mathf {
  virtual double operator()(double x) const { return x; }
};

// Parallelized code uses (thread-safe) RVector instead of NumericVector
struct Mathw : Worker {
  Mathw(const NumericVector input, Mathf const& mathf, NumericVector output)
    : input(input), mathf(mathf), output(output) {}
  void operator()(std::size_t begin, std::size_t end) {
    std::transform(input.begin() + begin, input.begin() + end, 
      output.begin() + begin, std::ref(mathf));
  }
private:
  const RVector<double> input;
  const Mathf& mathf;
  RVector<double> output;
};

// Perform the calculation (possibly in parallel) on a NumericVector
NumericVector math_(NumericVector x, Mathf const& mathf, const R_xlen_t para) {
  R_xlen_t n = x.length();
  NumericVector output = no_init(n);
  if (n < para) {
    for (R_xlen_t i = 0; i < n; ++i) {
      output[i] = mathf(x[i]);
    }
  } else {
    Mathw mathw(x, mathf, output);
    parallelFor(0, n, mathw);
  }
  DUPLICATE_ATTRIB(output, x);
  // TODO: Special treatment for label and units ?
  return output;
}

#define MATHBODY(funname) \
  if (Rf_isFactor(x) || is<Date>(x) || x.inherits("POSIXt") || \
  x.inherits("difftime") || is<ComplexVector>(x) || x.isS4()) { \
    stop("incompatible type or S4 object (use log() instead)"); \
  } else if (is<DataFrame>(x)) { \
    DataFrame df = clone(as<DataFrame>(x)); \
    RObject xi; \
    for (R_xlen_t i = 0; i < df.cols(); ++i) { \
      xi = df[i]; \
      if (Rf_isFactor(xi) || is<Date>(xi) || xi.inherits("POSIXt") || \
        xi.inherits("difftime") || is<ComplexVector>(xi) || xi.isS4()) { \
        stop("incompatible column %i type in data frame (use %s() instead)", \
          i + 1, #funname); \
      } else if (!is<NumericVector>(xi) && !is<IntegerVector>(xi) && \
        !is<LogicalVector>(xi)) { \
        stop("non-numeric column %i in the data frame", i + 1); \
      } \
      df[i] = math_(df[i], mathfun, para); \
    } \
    return df; \
  } else if (!is<NumericVector>(x) && !is<IntegerVector>(x) && \
    !is<LogicalVector>(x)) { \
    stop("non-numeric argument to mathematical function"); \
  } \
  return math_(as<NumericVector>(x), mathfun, para);

// log(x)
struct Logf: Mathf {
  Logf(double logbase) : Mathf(), logbase(logbase) {}
  double operator()(double x) const override { return log(x) / logbase; }
private:
  const double logbase;
};

//' @rdname Log_
//' @param base the base of the logarithm (e = `exp(1)` by default)
//' @export
// [[Rcpp::export]]
RObject log_(RObject x, double base = 2.71828182845904523536028747135266250,
    const R_xlen_t para = 5e4) {
  const double logbase = log(base);
  Logf mathfun(logbase);
  MATHBODY(log);
}

// Note: this was a simpler and faster version for log()  with n = 1e9, but it
// accepts only NumericVector and does not deal with data frames or attributes.
// NumericVector log_(NumericVector x,
//     double base = 2.71828182845904523536028747135266250,
//     const R_xlen_t para = 50000) {
//   const double logbase = log(base);
//   Logf mathfun(logbase);
//   R_xlen_t n = x.length();
//   NumericVector output = no_init(n);
//   if (x.length() < para) {
//     for (R_xlen_t i = 0; i < n; ++i) {
//       output[i] = mathfun(x[i]);
//     }
//   } else {// Parallel computation
//     Mathw mathw(x, mathfun, output);
//     parallelFor(0, n, mathw);
//   }
//   return output;
// }

// round(x)
struct Roundf: Mathf {
  Roundf(const int digits) : Mathf(), digits(digits) {}
  double operator()(double x) const override { return Rf_fround(x, digits); }
private:
  const int digits;
};

//' @rdname Round_
//' @description [round_()] rounds the values in its first argument to the
//' specified number of decimal places (default 0). See 'Details' about "round
//' to even" in [round()] when rounding off a 5.
//' @param digits integer indication the number of decimal places ([round_()])
//' or significant digits ([signif_()]) to be used. For [round_()], negative
//' values are allowed and indicate rounding to a power of ten (-2 means
//' rounding to the nearest hundred), see [round()].
//' @export
//' @seealso [round()], [signif()]
// [[Rcpp::export]]
RObject round_(RObject x, const int digits = 0, const R_xlen_t para = 5e4) {
  Roundf mathfun(digits);
  MATHBODY(round);
}

// signif(x)
struct Signiff: Mathf {
  Signiff(const int digits) : Mathf(), digits(digits) {}
  double operator()(double x) const override { return Rf_fprec(x, digits); }
private:
  const int digits;
};

//' @rdname Round_
//' @description [signif_()] rounds the values in its first argument to the
//' specified number of significant digits.
//' @export
// [[Rcpp::export]]
RObject signif_(RObject x, const int digits = 6, const R_xlen_t para = 5e4) {
  Signiff mathfun(digits);
  MATHBODY(signif);
}
