test_that("log_() gives same results as log()", {
  # Sequential version
  expect_equal(log_(1:5), log(1:5))
  expect_equal(log_(1:5, base = 2.5), log(1:5, base = 2.5))
  
  # Parallel version
  expect_equal(log_(1:40), log(1:40))
  expect_equal(log_(1:40, base = 3.3, paralen = 20), log(1:40, base = 3.3))
  
  # From here, we test only in sequential mode, since core code is the same
  # Edge cases
  expect_equal(log_(numeric(0)), numeric(0))
  expect_equal(log_(Inf), Inf)
  expect_equal(log_(-1), NaN)
  expect_equal(log_(-Inf), NaN)
  expect_equal(log_(NaN), NaN)
  expect_equal(log_(NA), NA_real_)
  
  # Default base is equal to exp(1)
  expect_equal(log_(1:5), log(1:5, base = exp(1)))
  
  # Wrong base
  expect_equal(log_(4, base = 0), 0)
  expect_equal(log_(4, base = -1), NaN)
  expect_equal(log_(4, base = Inf), 0)
  expect_equal(log_(4, base = -Inf), NaN)
  expect_error(log_(4, base = NULL))
  expect_error(log_(4, base = NULL))
  expect_error(log_(4, base = numeric(0)))
  expect_error(log_(4, base = "a"))
  
  # Wrong object type
  expect_error(log_(NULL))
  expect_error(log_("a"))
  
  # Delegation to log()
  expect_error(log_(as.factor(1:5)))
  expect_error(log_(Sys.Date()))
  expect_error(log_(Sys.time()))
  expect_error(log_(difftime(Sys.time(), Sys.time())))
  expect_equal(log_(as.complex(1:5)), log(as.complex(1:5)))
  
  # S4 object (also deleguated to log())
  setClass("CoordsTest", representation(lat = "numeric", long = "numeric"))
  coords <- new("CoordsTest", lat = 50.46339, long = 3.95528)
  # No method defined (yet) for this object
  expect_error(log_(coords))
  # But can apply on slots
  expect_equal(log_(coords@lat), log(coords@lat))
  
  # Data frame version
  # Sequential
  df <- data.frame(x = 1:30, y = 6:35)
  expect_equal(log_(df), log(df))
  # Parallel
  expect_equal(log_(df, paralen = 20), log(df))
  # If there is an incompatible column, this generates an error
  df$z <- as.factor(1:30)
  expect_error(log_(df))
  # TODO: data.table check
  
  # Handling of attributes (names, class, dim, ...)
  x <- structure(1:5, names = letters[1:5], class = "test", label = "some data",
    comment = "test", class = c("testdata", "numeric"))
  logx <- log_(x)
  expect_equal(attributes(logx), attributes(x))
  # Dimensions and dimnames of matrices must be kept too
  m <- matrix(1:12, ncol = 2)
  dimnames(m) <- list(letters[1:6], letters[7:8])
  logm <- log_(m)
  expect_equal(attributes(logm), attributes(m))
})
