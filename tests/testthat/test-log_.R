test_that("log_() gives same results as log()", {
  expect_equal(log_(1:5), log(1:5))
  expect_equal(log_(1:5, base = 2.5), log(1:5, base = 2.5))
  # Parallel version
  expect_equal(log_(1:40), log(1:40))
  expect_equal(log_(1:40, base = 3.3, paralen = 30), log(1:40, base = 3.3))
})
