context('config')

test_that('new_av_state() defines av_state correctly', {
  av_state <- new_av_state()
  expect_true(is(av_state ,'av_state'))
})

test_that('data_test.sav loads properly',{
  av_state <- NULL
  capture.output({
    av_state <- load_file('data_test.sav')
  })
  expect_match(av_state$file_name,'data_test.sav')
  expect_equal(av_state$real_file_name,'data_test.sav')
  expect_equal(av_state$file_type,'SPSS')
  expect_equivalent(av_state$raw_data,generate_test_data())
  expect_equivalent(av_state$data[['multiple']],generate_test_data())  
})

test_that('load_test_data() is equivalent to loading data_test.sav',{
  # it actually isnt. loading the file sets a column called 'multiple'.
  # In load_test_data(), the data is labeled under '1'.
  av_state <- load_test_data()
  expect_match(av_state$file_name,'data_test.sav')
  expect_equal(av_state$real_file_name,'data_test.sav')
  expect_equal(av_state$file_type,'SPSS')
  expect_equal(av_state$raw_data,generate_test_data())
  expect_equal(av_state$data[[1]],generate_test_data())  
})
