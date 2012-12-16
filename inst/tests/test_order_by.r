context('order_by')

test_that('order_by() sets the order_by attribute',{
  load_test_data()
  capture.output({
    group_by('id')
  })
  expect_true(is.null(av_state$order_by))
  capture.output({
    order_by('tijdstip')
  })
  expect_equal(av_state$order_by,'tijdstip')
})

test_that('order_by() can only be called once',{
  load_test_data()
  capture.output({
    order_by('id')
  })
  expect_error(order_by('id'),'called once')
})

test_that('order_by() can only be with a numeric id_field',{
  load_test_data()
  expect_error(order_by('home'),'has to be numeric')
})

test_that('order_by() does ADD_MISSING imputation and ordering correctly',{
  load_test_data()
  av_state$data[[1]] <<- data.frame(id=rep(1,times=5),tijdstip=c(1,3,5,6,7),home=c('yes','no','yes',NA,'yes'))
  capture.output({
    group_by('id')
  })
  expect_output(order_by('tijdstip'),'ADD_MISSING')
  expect_equal(av_state$data[[1]]$tijdstip,1:7)
  expect_equal(av_state$data[[1]]$id,rep(1,times=7))
  load_test_data()
  av_state$data[[1]] <<- data.frame(id=rep(1,times=5),tijdstip=c(7,3,5,6,1),home=c('yes','no','yes',NA,'yes'))
  expect_output(order_by('tijdstip'),'ADD_MISSING')
  expect_equal(av_state$data[[1]]$tijdstip,1:7)
})

test_that('order_by() does ONE_MISSING imputation and ordering correctly',{
  load_test_data()
  av_state$data[[1]] <<- data.frame(id=rep(1,times=5),tijdstip=c(5,3,NA,1,2),home=c('yes','no','yes',NA,'yes'))
  expect_output(order_by('tijdstip'),'ONE_MISSING')
  expect_equal(av_state$data[[1]]$tijdstip,1:5)
  expect_equal(av_state$data[[1]]$home,factor(c(NA,'yes','no','yes','yes')))
})

test_that('order_by() does NONE imputation and ordering correctly',{
  load_test_data()
  av_state$data[[1]] <<- data.frame(id=rep(1,times=5),tijdstip=c(5,3,4,1,2),home=c('yes','no','yes',NA,'yes'))
  expect_output(order_by('tijdstip'),'^$')
  expect_equal(av_state$data[[1]]$tijdstip,1:5)
  expect_equal(av_state$data[[1]]$home,factor(c(NA,'yes','no','yes','yes')))
})
