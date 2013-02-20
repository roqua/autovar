context('group_by')

test_that('id_field is a required parameter',{
  av_state <- load_test_data()
  expect_error(group_by(av_state),"missing id_field parameter")
})

test_that('group_by() sets the group_by flag in av_state',{
  av_state <- load_test_data()
  expect_true(is.null(av_state$group_by))
  capture.output({
    av_state <- group_by(av_state,'id')
  })
  expect_equal(av_state$group_by,'id')
})

test_that('group_by() can only be called once',{
  av_state <- load_test_data()
  capture.output({
    av_state <- group_by(av_state,'id')
  })
  expect_error(group_by(av_state,'id'),'called once')
})

test_that('split_up makes two subsets when there are two distinct values for the id_field',{
  a <- data.frame(id=c(2,rep(1,times=3),2),tijdstip=c(1,3,5,6,7),home=c('yes','no','yes',NA,'yes'))  
  b <- split_up(a,'id')
  d <- list()
  d[['1']] <- a[c(2,3,4),]
  d[['2']] <- a[c(1,5),]
  expect_equal(b,d)
})

test_that('split_up filters out any NA values for id_field into a separate subset',{
  a <- data.frame(id=c(NA,rep(1,times=3),NA),tijdstip=c(1,3,5,6,7),home=c('yes','no','yes',NA,'yes'))  
  b <- split_up(a,'id')
  d <- list()
  d[['1']] <- a[c(2,3,4),]
  d[['NA']] <- a[c(1,5),]
  expect_equal(b,d)
})

test_that('split_up doesnt sort rows in subsets but does sort its subset by id_field',{
  a <- data.frame(id=c(2,rep(1,times=3),2),tijdstip=c(7,3,5,6,1),home=c('yes','no','yes',NA,'yes'))  
  b <- split_up(a,'id')
  d <- list()
  d[['1']] <- a[c(2,3,4),]
  d[['2']] <- a[c(1,5),]
  expect_equal(b,d)
})
