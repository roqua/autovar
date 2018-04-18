context("evaluate model")

context(" estimate_var_model")
test_that("it should run the vars VAR function with the provided arguments ", {
  cur.data <- data.frame(a = rnorm(10,0,1), b = runif(10,0,1))
  cur.lag <- 10
  called_count <<- 0
  with_mock(
    `vars::VAR` = function(data, p, ...) {
      expect_equal(data, cur.data)
      expect_equal(p, cur.lag)
      called_count <<- called_count + 1
      1
    },
    expect_equal(estimate_var_model(cur.data, cur.lag), 1)
  )
  expect_equal(called_count, 1)
})

test_that("it should run the vars VAR function without the dots if they are all NULL", {
  cur.data <- data.frame(a = rnorm(10,0,1), b = runif(10,0,1))
  cur.lag <- 10
  called_count <<- 0
  with_mock(
    `vars::VAR` = function(data, p, ...) {
      expect_equal(data, cur.data)
      expect_equal(p, cur.lag)
      called_count <<- called_count + 1
      expect_equal(list(...), list())
      1
    },
    expect_equal(estimate_var_model(cur.data, cur.lag, exogen = NULL, whatevermat = NULL), 1)
  )
  expect_equal(called_count, 1)
})

test_that("it should run the vars VAR function without the dots if they are not provided", {
  cur.data <- data.frame(a = rnorm(10,0,1), b = runif(10,0,1))
  cur.lag <- 10
  called_count <<- 0
  with_mock(
    `vars::VAR` = function(data, p, ...) {
      expect_equal(data, cur.data)
      expect_equal(p, cur.lag)
      called_count <<- called_count + 1
      expect_equal(list(...), list())
      1
    },
    expect_equal(estimate_var_model(cur.data, cur.lag), 1)
  )
  expect_equal(called_count, 1)
})

test_that("it should run the vars VAR function with the dots if they are not all NULL", {
  cur.data <- data.frame(a = rnorm(10,0,1), b = runif(10,0,1))
  cur.lag <- 10
  called_count <<- 0
  with_mock(
    `vars::VAR` = function(data, p, ...) {
      expect_equal(data, cur.data)
      expect_equal(p, cur.lag)
      called_count <<- called_count + 1
      expect_equal(list(...), list(exogen = 1234, whatevermat = NULL))
      1
    },
    expect_equal(estimate_var_model(cur.data, cur.lag, exogen = 1234, whatevermat = NULL), 1)
  )
  expect_equal(called_count, 1)
})
