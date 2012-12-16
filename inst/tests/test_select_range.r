context('select_range')

test_that('select_range only accepts valid subset ids',{
  load_test_data()
  expect_error(select_range(2,column='tijdstip',begin=1),'does not identify a data set')
  expect_error(select_range(0,column='tijdstip',begin=1),'does not identify a data set')
  expect_error(select_range(1.5,column='tijdstip',begin=1),'does not identify a data set')
  expect_error(select_range('2',column='tijdstip',begin=1),'does not identify a data set')
  expect_error(select_range('1',column='tijdstip',begin=1),'does not identify a data set')
})

test_that('select_range needs a column argument',{
  load_test_data()
  expect_error(select_range(1,begin=1),'a column argument')
  capture.output({
    order_by('tijdstip')
  })
  expect_error(select_range(1),'either begin or end')
})

test_that('select_range only accepts valid columns',{
  load_test_data()
  expect_error(select_range(1,column='nonexistant',begin=1),'column does not exist')
})

test_that('select_range needs either begin or end specified',{
  load_test_data()
  expect_error(select_range(1,column='tijdstip'),'either begin or end')
})

test_that('select_range with begin works correctly',{
  load_test_data()
  a <- data.frame(id=rep(1,times=5),tijdstip=c(7,3,5,6,1),
                  home=c('yes','no','yes',NA,'yes'))
  av_state$data[[1]] <<- a
  capture.output({
    select_range('multiple','tijdstip',begin=5)
  })
  b <- a[c(1,3,4),]
  expect_equal(av_state$data[[1]],b)
})

test_that('select_range with end works correctly',{
  load_test_data()
  a <- data.frame(id=rep(1,times=5),tijdstip=c(7,3,5,6,1),
                  home=c('yes','no','yes',NA,'yes'))
  av_state$data[[1]] <<- a
  capture.output({
    select_range('multiple','tijdstip',end=5)
  })
  b <- a[c(2,3,5),]
  expect_equal(av_state$data[[1]],b)
})

test_that('select_range with begin and end works correctly',{
  load_test_data()
  a <- data.frame(id=rep(1,times=5),tijdstip=c(7,3,5,6,1),
                  home=c('yes','no','yes',NA,'yes'))
  av_state$data[[1]] <<- a
  capture.output({
    select_range('multiple','tijdstip',begin=6,end=7)
  })
  b <- a[c(1,4),]
  expect_equal(av_state$data[[1]],b)
})
