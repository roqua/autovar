context('sort_models')

test_that('models are sorted correctly',{
  load('av_state.RData')
  rejected_models <- sort_models(av_state$rejected_models)
  expect_equal(length(rejected_models),length(av_state$rejected_models))
  prev <- 0
  for (model in rejected_models) {
    expect_true(prev <= model_score(model$varest))
    prev <- model_score(model$varest)
  }
})
