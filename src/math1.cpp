/**
 * A parallel version of various math functions that accept a single argument.
 */

// TODO: add an inplace = FALSE argument... When TRUE, change the vector or data
// frame in place (for data frame, just do not clone)
// 
// TODO: list of other functions to implements:
// follow here: https://homepage.divms.uiowa.edu/~luke/R/experimental/
// and here: https://github.com/atks/Rmath/blob/master/Rmath/Rmath.h
// and here: https://github.com/wch/r-source/blob/trunk/src/main/names.c
// and here for C functions: https://koor.fr/C/cmath/round.wp
// For RcppParallel code for runif() and rnorm(), see rTRNG and dqrng
// for step() in xoshiro,
// see https://github.com/degski/xoroshiro/tree/master/xoroshiro
// and https://gitlab.com/nssn/xoshiro/-/blob/main/include/xoshiro.h?ref_type=heads

// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>
#include <cmath>
#include <algorithm>

using namespace Rcpp;
using namespace RcppParallel;

// pointer to the function to use
double (*math1fun)(double);

struct Math1w : Worker {
  Math1w(const NumericVector input, NumericVector output) 
    : input(input), output(output) {}
  void operator()(std::size_t begin, std::size_t end) {
    std::transform(input.begin() + begin, input.begin() + end, 
      output.begin() + begin, ::math1fun);
  }
private:
  const RVector<double> input;
  RVector<double> output;
};

// If we prefer to delegate to base R equivalent function:
//Function f(#funname);
//return f(x);
// TODO: Special treatment for data.tables before return df;
// because data.table:::selfrefok(df) is 0 now -> not good
#define MATH1BODY(funname) \
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
        stop("incompatible column %i type in data frame (use log() instead)", \
          i + 1); \
      } else if (!is<NumericVector>(xi) && !is<IntegerVector>(xi) && \
        !is<LogicalVector>(xi)) { \
        stop("non-numeric column %i in the data frame", i + 1); \
      } \
      df[i] = funname##_(df[i], para); \
    } \
    return df; \
  } else if (!is<NumericVector>(x) && !is<IntegerVector>(x) && \
    !is<LogicalVector>(x)) { \
    stop("Non-numeric argument to mathematical function"); \
  } \
  NumericVector xnum = as<NumericVector>(x); \
  R_xlen_t n = xnum.length(); \
  NumericVector output = no_init(n); \
  if (n < para) { \
    for (R_xlen_t i = 0; i < n; ++i) { \
      output[i] = math1fun(xnum[i]); \
    } \
  } else { \
    Math1w math1w(xnum, output); \
    parallelFor(0, n, math1w); \
  } \
  DUPLICATE_ATTRIB(output, x); \
  return output;

// TODO: add round_() and signif_() that take two arguments
//' Rounding of Numbers (Fast Parallel Version)
//' 
//' @name Round_
//' @description
//' Fast version of rounding (when vector size >= 50000).
//' [ceiling_()] takes a single numeric argument x and returns a numeric vector
//' containing the smallest integers not less than the corresponding elements of
//' `x`. It is similar to [ceiling()].
//' 
//' [floor_()] takes a single numeric argument `x` and returns a numeric vector
//' containing the largest integers not greater than the corresponding elements of
//' `x`. It is similar to [floor()].
//' 
//' [trunc_()] takes a single numeric argument `x` and returns a numeric vector
//' containing the integers formed by truncating the values in `x` toward 0. It
//' is similar to [trunc()].
//'
//' @param x vector of numeric values
//' @param para the minimum length of `x` to use parallel computation (50000
//' by default)
//' 
//' @return A numeric vector, matrix, or data frame with the transformed values.
//' 
//' @details
//' They are **not** generic functions and do not process factor, Date, POSIXt,
//' difftime, complex, or S4 objects (use base R equivalent function instead).
//' Data frames are processed column-wise, providing each column is compatible.
//' All attributes are preserved.
//' 
//' @export
//' @seealso [ceiling()], [floor()], [trunc()]
//' @examples
//' floor_(c(1.23, 4.56, -7.89))
// [[Rcpp::export]]
 RObject ceiling_(RObject x, const R_xlen_t para = 5e4) {
  math1fun = &std::ceil;
  MATH1BODY(ceiling)
}

//' @rdname Round_
//' @export
// [[Rcpp::export]]
RObject floor_(RObject x, const R_xlen_t para = 5e4) {
  math1fun = &std::floor;
  MATH1BODY(floor)
}

//' @rdname Round_
//' @export
// [[Rcpp::export]]
RObject trunc_(RObject x, const R_xlen_t para = 5e4) {
  math1fun = &std::trunc;
  MATH1BODY(trunc)
}

//' Trigonometric Functions (Fast Parallel Version)
//' 
//' @name Trig_
//' @description
//' Fast version of trigonometric functions (when vector size >= 50000). They
//' respectively compute the cosine, sine, tangent, arc-cosine, arc-sine,
//' arc-tangent, and the two-argument arc-tangent.
//' 
//'  `cospi_(x)`, `sinpi_(x)`, and `tanpi_(x)`, compute `cos(pi*x)`,
//'  `sin(pi*x)`, and `tan(pi*x)`.
//'
//' @param x vector of numeric values
//' @param para the minimum length of `x` to use parallel computation (50000
//' by default)
//' 
//' @return A numeric vector, matrix, or data frame with the transformed values.
//' 
//' @details
//' They are **not** generic functions and do not process factor, Date, POSIXt,
//' difftime, complex, or S4 objects (use base R equivalent function instead).
//' Data frames are processed column-wise, providing each column is compatible.
//' All attributes are preserved.
//' 
//' @export
//' @seealso [cos()], [sin()], [tan()], [acos()], [asin()], [atan()], [atan2()]
//' @examples
//' cos_(1:5)
// [[Rcpp::export]]
RObject cos_(RObject x, const R_xlen_t para = 5e4) {
  math1fun = &std::cos;
  MATH1BODY(cos)
}

//' @rdname Trig_
//' @export
// [[Rcpp::export]]
RObject sin_(RObject x, const R_xlen_t para = 5e4) {
  math1fun = &std::sin;
  MATH1BODY(sin)
}

//' @rdname Trig_
//' @export
// [[Rcpp::export]]
RObject tan_(RObject x, const R_xlen_t para = 5e4) {
  math1fun = &std::tan;
  MATH1BODY(tan)
}

//' @rdname Trig_
//' @export
// [[Rcpp::export]]
RObject acos_(RObject x, const R_xlen_t para = 5e4) {
  math1fun = &std::acos;
  MATH1BODY(acos)
}

//' @rdname Trig_
//' @export
// [[Rcpp::export]]
RObject asin_(RObject x, const R_xlen_t para = 5e4) {
  math1fun = &std::asin;
  MATH1BODY(asin)
}

//' @rdname Trig_
//' @export
// [[Rcpp::export]]
RObject atan_(RObject x, const R_xlen_t para = 5e4) {
  math1fun = &std::atan;
  MATH1BODY(atan)
}

// This one does not work because it asks for two vectors x and y!
//  //' @rdname Trig_
//  //' @export
//  // [[Rcpp::export]]
//RObject atan2_(RObject y, RObject x, const R_xlen_t para = 5e4) {
//  math1fun = &std::atan2;
//  MATH1BODY(atan2)
//}

double cospi(double x) {
  return std::cos(M_PI * x);
}
//' @rdname Trig_
//' @export
// [[Rcpp::export]]
RObject cospi_(RObject x, const R_xlen_t para = 5e4) {
  math1fun = &cospi;
  MATH1BODY(cospi)
}

double sinpi(double x) {
  return std::sin(M_PI * x);
}
//' @rdname Trig_
//' @export
// [[Rcpp::export]]
RObject sinpi_(RObject x, const R_xlen_t para = 5e4) {
  math1fun = &sinpi;
  MATH1BODY(sinpi)
}

double tanpi(double x) {
  return std::tan(M_PI * x);
}
//' @rdname Trig_
//' @export
// [[Rcpp::export]]
RObject tanpi_(RObject x, const R_xlen_t para = 5e4) {
  math1fun = &tanpi;
  MATH1BODY(tanpi)
}

//' Logarithm (Fast Parallel Version)
//' 
//' @name Log_
//' @description
//' Fast version of logarithmic and exponential functions (when vector size >=
//' 50000). [log_()] computes the natural logarithm of `x` (base `e` by default),
//' [log2_()] computes the base 2 logarithm, [log10_()] computes the base 10.
//' [log1p_()] computes `log(1 + x)` accurately even for small `x`.
//' 
//' [exp_()] computes the exponential function. [expm1_()] computes `exp(x) - 1`
//' accurately even for small `x`.
//'
//' @param x vector of numeric values
//' @param para the minimum length of `x` to use parallel computation (50000
//' by default)
//' 
//' @return A numeric vector, matrix, or data frame with the transformed values.
//' 
//' @details
//' They are **not** generic functions and do not process factor, Date, POSIXt,
//' difftime, complex, or S4 objects (use base R equivalent function instead).
//' Data frames are processed column-wise, providing each column is compatible.
//' All attributes are preserved.
//' 
//' @export
//' @seealso [log10()], [log2()], [log()], [log1p()], [exp()], [expm1()]
//' @examples
//' log_(1:5)
//' log_(1:5, base = 2.5)
// [[Rcpp::export]]
RObject log10_(RObject x, const R_xlen_t para = 5e4) {
  math1fun = &std::log10;
  MATH1BODY(log10)
}

//' @rdname Log_
//' @export
// [[Rcpp::export]]
RObject log2_(RObject x, const R_xlen_t para = 5e4) {
  math1fun = &std::log2;
  MATH1BODY(log2)
}

//' @rdname Log_
//' @export
// [[Rcpp::export]]
RObject log1p_(RObject x, const R_xlen_t para = 5e4) {
  math1fun = &std::log1p;
  MATH1BODY(log1p)
}

//' @rdname Log_
//' @export
// [[Rcpp::export]]
RObject exp_(RObject x, const R_xlen_t para = 5e4) {
  math1fun = &std::exp;
  MATH1BODY(exp)
}

//' @rdname Log_
//' @export
// [[Rcpp::export]]
RObject expm1_(RObject x, const R_xlen_t para = 5e4) {
  math1fun = &std::expm1;
  MATH1BODY(expm1)
}

