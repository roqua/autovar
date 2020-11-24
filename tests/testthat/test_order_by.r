context('order_by')

test_that('order_by() sets the order_by attribute',{
  av_state <- load_test_data()
  capture.output({
    av_state <- group_by(av_state,'id')
  })
  expect_true(is.null(av_state$order_by))
  capture.output({
    av_state <- order_by(av_state,'tijdstip')
  })
  expect_equal(av_state$order_by,'tijdstip')
})

test_that('order_by() can only be called once',{
  av_state <- load_test_data()
  capture.output({
    av_state <- order_by(av_state,'tijdstip')
  })
  expect_error(order_by(av_state,'tijdstip'),'called once')
})

test_that('order_by() can only be with a numeric id_field',{
  av_state <- load_test_data()
  expect_error(order_by(av_state,'home'),'has to be numeric')
})

test_that('order_by() prints a warning when called with a sequence it cant complete',{
  av_state <- load_test_data()
  av_state$data[[1]]$tijdstip <- c(1,2,NA,5,6)
  expect_warning(order_by(av_state,'tijdstip'),'are still NA')
  av_state <- load_test_data()
  av_state$data[[1]]$tijdstip <- c(1,2,NA,5,6)
  expect_warning(order_by(av_state,'tijdstip'),'are still nonsequential')
})

test_that('order_by() does ADD_MISSING imputation and ordering correctly',{
  av_state <- load_test_data()
  av_state$data[[1]] <- data.frame(id=rep(1,times=5),tijdstip=c(1,3,5,6,7),home=c('yes','no','yes',NA,'yes'), stringsAsFactors = TRUE)
  capture.output({
    av_state <- group_by(av_state,'id')
  })
  expect_output({av_state <- order_by(av_state,'tijdstip')},'ADD_MISSING')
  expect_equal(av_state$data[[1]]$tijdstip,1:7)
  expect_equal(av_state$data[[1]]$id,rep(1,times=7))
  av_state <- load_test_data()
  av_state$data[[1]] <- data.frame(id=rep(1,times=5),tijdstip=c(7,3,5,6,1),home=c('yes','no','yes',NA,'yes'), stringsAsFactors = TRUE)
  expect_output({av_state <- order_by(av_state,'tijdstip')},'ADD_MISSING')
  expect_equal(av_state$data[[1]]$tijdstip,1:7)
})

test_that('order_by() does ONE_MISSING imputation and ordering correctly',{
  av_state <- load_test_data()
  av_state$data[[1]] <- data.frame(id=rep(1,times=5),tijdstip=c(5,3,NA,1,2),home=c('yes','no','yes',NA,'yes'), stringsAsFactors = TRUE)
  expect_output({av_state <- order_by(av_state,'tijdstip')},'ONE_MISSING')
  expect_equal(av_state$data[[1]]$tijdstip,1:5)
  expect_equal(av_state$data[[1]]$home,factor(c(NA,'yes','no','yes','yes')))
})

test_that('order_by() does NONE imputation and ordering correctly',{
  av_state <- load_test_data()
  av_state$data[[1]] <- data.frame(id=rep(1,times=5),tijdstip=c(5,3,4,1,2),home=c('yes','no','yes',NA,'yes'), stringsAsFactors = TRUE)
  expect_output({av_state <- order_by(av_state,'tijdstip')},NA)
  expect_equal(av_state$data[[1]]$tijdstip,1:5)
  expect_equal(av_state$data[[1]]$home,factor(c(NA,'yes','no','yes','yes')))
})
