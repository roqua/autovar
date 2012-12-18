context('add_derived_column')

test_that('ln only accepts existing columns',{
  load_test_data()
  expect_error(add_derived_column('new','nonexistant',operation='LN'),'does not exist')
})

test_that('ln only accepts numeric columns',{
  load_test_data()
  expect_error(add_derived_column('new','home',operation='LN'),'is not numeric')
})

test_that('ln gives the correct output',{
  load_test_data()
  add_derived_column('new','tijdstip',operation='LN')
  expect_equivalent(av_state$data[[1]],cbind(generate_test_data(),log(generate_test_data()$tijdstip)))
})

test_that('mtoh only accepts existing columns',{
  load_test_data()
  expect_error(add_derived_column('new','nonexistant',operation='MINUTES_TO_HOURS'),'does not exist')
})

test_that('mtoh only accepts numeric columns',{
  load_test_data()
  expect_error(add_derived_column('new','home',operation='MINUTES_TO_HOURS'),'is not numeric')
})

test_that('mtoh gives the correct output',{
  load_test_data()
  add_derived_column('new','tijdstip',operation='MINUTES_TO_HOURS')
  expect_equivalent(av_state$data[[1]],cbind(generate_test_data(),(generate_test_data()$tijdstip/60)))
})

test_that('sum only accepts existing columns',{
  load_test_data()
  expect_error(add_derived_column('new','nonexistant',operation='SUM'),'does not exist')
})

test_that('sum gives the correct output for numeric columns',{
  load_test_data()
  add_derived_column('new',c('id','tijdstip'),operation='SUM')
  expect_equivalent(av_state$data[[1]],cbind(generate_test_data(),generate_test_data()$tijdstip+1))
})

test_that('sum gives the correct output for nominal columns',{
  load_test_data()
  expect_output(add_derived_column('new',c('home','home'),operation='SUM'),'converting')
  expect_equivalent(av_state$data[[1]],cbind(generate_test_data(),c(2,0,2,0,2)))
})
