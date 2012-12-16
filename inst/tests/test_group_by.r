context('group_by')

test_that('id_field is a required parameter',{
  expect_error(group_by(),"missing id_field parameter")
})

test_that('group_by() sets the group_by flag in av_state',{
  load_test_data()
  expect_true(is.null(av_state$group_by))
  capture.output({
    group_by('id')
  })
  expect_equal(av_state$group_by,'id')
})

test_that('group_by() can only be called once',{
  load_test_data()
  capture.output({
    group_by('id')
  })
  expect_error(group_by('id'),'called once')
})

test_that('split_up makes two subsets when there are two distinct values for the id_field',{
  a <- data.frame(id=c(2,rep(1,times=3),2),tijdstip=c(1,3,5,6,7),home=c('yes','no','yes',NA,'yes'))  
  b <- split_up(a,'id')
  d <- list()
  d[['1']] <- data.frame(id=rep(1,times=3),tijdstip=c(3,5,6),home=c('no','yes',NA))
  d[['2']] <- data.frame(id=rep(2,times=2),tijdstip=c(1,7),home=factor(c('yes','yes'),levels=c('no','yes')))
  expect_equivalent(b,d)
})

test_that('split_up filters out any NA values for id_field in a separate subset',{
  a <- data.frame(id=c(NA,rep(1,times=3),NA),tijdstip=c(1,3,5,6,7),home=c('yes','no','yes',NA,'yes'))  
  b <- split_up(a,'id')
  d <- list()
  d[['1']] <- data.frame(id=rep(1,times=3),tijdstip=c(3,5,6),home=c('no','yes',NA))
  d[['2']] <- data.frame(id=as.numeric(rep(NA,times=2)),tijdstip=c(1,7),home=factor(c('yes','yes'),levels=c('no','yes')))
  expect_equivalent(b,d)
})

test_that('split_up doesnt sort rows in subsets but does sort its subset by id_field',{
  a <- data.frame(id=c(2,rep(1,times=3),2),tijdstip=c(7,3,5,6,1),home=c('yes','no','yes',NA,'yes'))  
  b <- split_up(a,'id')
  d <- list()
  d[['1']] <- data.frame(id=rep(1,times=3),tijdstip=c(3,5,6),home=c('no','yes',NA))
  d[['2']] <- data.frame(id=rep(2,times=2),tijdstip=c(7,1),home=factor(c('yes','yes'),levels=c('no','yes')))
  expect_equivalent(b,d)
})
