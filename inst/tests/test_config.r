context('config')

test_that('print_state() is defined and equal to print(av_state)', {
  capture.output({
    a <- print_state()
    b <- print(av_state)
  })
  expect_equal(a,b)
})

test_that('reset_state() defines av_state correctly', {
  expect_true(exists(envir=.GlobalEnv,'av_state'))
  expect_equal(class(av_state),'av_state')
  rm('av_state',envir=.GlobalEnv)
  expect_false(exists(envir=.GlobalEnv,'false'))
  expect_error(class(av_state),'not found')
  reset_state()
  expect_true(exists(envir=.GlobalEnv,'av_state'))
  expect_equal(class(av_state),'av_state')
})

test_that('data_test.sav loads properly',{
  capture.output({
    load_file('data_test.sav')
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
  load_test_data()
  expect_match(av_state$file_name,'data_test.sav')
  expect_equal(av_state$real_file_name,'data_test.sav')
  expect_equal(av_state$file_type,'SPSS')
  expect_equivalent(av_state$raw_data,generate_test_data())
  expect_equivalent(av_state$data[['1']],generate_test_data())  
})
