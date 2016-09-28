context('create_model')

test_that('create_model works correctly',{
  a <- create_model(NULL,test=1,something='two')
  expect_equal(class(a),'var_model')
  expect_equal(length(a),2)
  expect_equal(a$test,1)
  expect_equal(a$something,'two')
})

test_that('merged properties work',{
  a <- create_model(NULL,a=1,b=2)
  b <- create_model(a,b=3,c=4)
  expect_equal(length(b),3)
  expect_equal(b$a,1)
  expect_equal(b$b,3)
  expect_equal(b$c,4)
})

test_that('complex objects work',{
  a <- create_model(NULL,a=list(b=5,c=7),b=data.frame(a=c(1,2),b=c(3,4)))
  b <- create_model(a,
                    b=data.frame(c=c(2,1,3),
                                 d=c(4,3,2),
                                 e=c('a','b','c')),
                    c=list(a=5,b='something',c=4))
  expect_equal(length(b),3)
  expect_equal(b$a,list(b=5,c=7))
  expect_equal(b$b,data.frame(c=c(2,1,3),
                              d=c(4,3,2),
                              e=c('a','b','c')))
  expect_equal(b$c,list(a=5,b='something',c=4))
})
