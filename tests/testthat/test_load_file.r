context('load_file')

test_that('file type SPSS is correctly detected',{
  av_state <- NULL
  capture.output({
    av_state <- load_file('data_test.sav')
  })
  expect_equal(av_state$file_type,'SPSS')
})

test_that('file type SPSS is correctly detected for opencpu files',{
  a <- 'data_testsav'
  attr(a,'filename') <- 'data_test.sav'
  av_state <- NULL
  expect_output({av_state <- load_file(a)},"id.*?tijdstip.*?home")
  expect_equal(av_state$file_type,'SPSS')
  expect_equal(av_state$real_file_name,'data_test.sav')
  expect_match(av_state$file_name,'data_testsav')
})

test_that('non-existant files generate an error message',{
  expect_error(load_file('doesnotexist.sav'),'File does not exist')
})
