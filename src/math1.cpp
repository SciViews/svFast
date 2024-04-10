/**
 * A parallel version of various math functions that accept a single argument.
 */

// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>
#include <cmath>
#include <algorithm>

using namespace Rcpp;
using namespace RcppParallel;

// pointer to the function to use
double (*math1fun)(double);

struct Math1w : public Worker {
  const RVector<double> input;
  RVector<double> output;
public:
  Math1w(const NumericVector input, NumericVector output) 
    : input(input), output(output) {}
  void operator()(std::size_t begin, std::size_t end) {
    std::transform(input.begin() + begin, input.begin() + end, 
      output.begin() + begin, ::math1fun);
  }
};

#define MATH1BODY(funname) \
  if (Rf_isFactor(x) || is<Date>(x) || x.inherits("POSIXt") || \
    x.inherits("difftime") || is<ComplexVector>(x) || x.isS4()) { \
    Function f(#funname); \
    return f(x); \
  } else if (is<DataFrame>(x)) { \
    DataFrame df = clone(as<DataFrame>(x)); \
    for (R_xlen_t i = 0; i < df.cols(); ++i) { \
      df[i] = funname##_(df[i], paralen); \
    } \
    return df; \
  } else if (!is<NumericVector>(x) && !is<IntegerVector>(x) && \
    !is<LogicalVector>(x)) { \
    stop("Non-numeric argument to mathematical function"); \
  } \
  NumericVector xnum = as<NumericVector>(x); \
  R_xlen_t n = xnum.length(); \
  NumericVector output = no_init(n); \
  if (n < paralen) { \
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
//' @param paralen the minimum length of `x` to use parallel computation (50000
//' by default)
//' 
//' @return A numeric vector, matrix, or data frame with the transformed values.
//' 
//' @details
//' They are **not** generic functions.
//' In case of factor, Date, POSIXt, difftime, complex, or S4 objects, the
//' function delegates to the base function. For data frames, no dispatching is
//' done and direct column-wise computation is performed. All attributes are
//' preserved.
//' 
//' @export
//' @seealso [ceiling()], [floor()], [trunc()]
//' @examples
//' floor_(c(1.23, 4.56, -7.89))
// [[Rcpp::export]]
 RObject ceiling_(RObject x, const R_xlen_t paralen = 5e4) {
  math1fun = &std::ceil;
  MATH1BODY(ceiling)
}

//' @rdname Round_
//' @export
// [[Rcpp::export]]
RObject floor_(RObject x, const R_xlen_t paralen = 5e4) {
  math1fun = &std::floor;
  MATH1BODY(floor)
}

//' @rdname Round_
//' @export
// [[Rcpp::export]]
RObject trunc_(RObject x, const R_xlen_t paralen = 5e4) {
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
//' @param paralen the minimum length of `x` to use parallel computation (50000
//' by default)
//' 
//' @return A numeric vector, matrix, or data frame with the transformed values.
//' 
//' @details
//' They are **not** generic functions.
//' In case of factor, Date, POSIXt, difftime, complex, or S4 objects, the
//' function delegates to the base function. For data frames, no dispatching is
//' done and direct column-wise computation is performed. All attributes are
//' preserved.
//' 
//' @export
//' @seealso [cos()], [sin()], [tan()], [acos()], [asin()], [atan()], [atan2()]
//' @examples
//' cos_(1:5)
// [[Rcpp::export]]
RObject cos_(RObject x, const R_xlen_t paralen = 5e4) {
  math1fun = &std::cos;
  MATH1BODY(cos)
}

//' @rdname Trig_
//' @export
// [[Rcpp::export]]
RObject sin_(RObject x, const R_xlen_t paralen = 5e4) {
  math1fun = &std::sin;
  MATH1BODY(sin)
}

//' @rdname Trig_
//' @export
// [[Rcpp::export]]
RObject tan_(RObject x, const R_xlen_t paralen = 5e4) {
  math1fun = &std::tan;
  MATH1BODY(tan)
}
