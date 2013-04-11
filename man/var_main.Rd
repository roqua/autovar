\name{var_main}
\alias{var_main}
\title{Determine possibly optimal models for Vector Autoregression}
\usage{
  var_main(av_state, vars, lag_max = 2,
    significance = 0.05, exogenous_max_iterations = 3,
    subset = 1, log_level = av_state$log_level,
    small = FALSE, include_model = NULL,
    exogenous_variables = NULL, use_sktest = TRUE,
    test_all_combinations = FALSE,
    restrictions.verify_validity_in_every_step = TRUE,
    restrictions.extensive_search = TRUE,
    criterion = c("AIC", "BIC"))
}
\arguments{
  \item{av_state}{an object of class \code{av_state}}

  \item{vars}{the vector of variables on which to perform
  vector autoregression. These should be the names of
  existing columns in the data sets of \code{av_state}.}

  \item{lag_max}{limits the highest possible number of lags
  that will be used in a model. This number sets the
  maximum limit in the search for optimal lags.}

  \item{significance}{the maximum P-value for which results
  are seen as significant. This argument is used in Granger
  causality tests, Portmanteau tests, and Jarque-Bera
  tests.}

  \item{exogenous_max_iterations}{determines how many times
  we should try to exclude additional outliers for a
  variable. This argument should be a number between 1 and
  3: \itemize{ \item \code{1} - When Jarque-Bera tests
  fail, having \code{exogenous_max_iterations = 1} will
  only try with removing 3.5x std. outliers for the
  residuals of variables using exogenous dummy variables.
  \item \code{2} - When \code{exogenous_max_iterations =
  2}, the program will also try with removing 3x std.
  outliers if JB tests still fail. \item \code{3} - When
  \code{exogenous_max_iterations = 3}, the program will
  also try with removing 2.5x std. outliers if JB tests
  still fail. }}

  \item{subset}{specifies which data subset the VAR
  analysis should run on. The VAR analysis only runs on one
  data subset at a time. If not specified, the first subset
  is used (corresponding to \code{av_state$data[[1]]}).}

  \item{log_level}{sets the minimum level of output that
  should be shown. It should be a number between 0 and 3. A
  lower level means more verbosity. \code{0} = debug,
  \code{1} = test detail, \code{2} = test outcomes,
  \code{3} = normal. The default is set to the value of
  \code{av_state$log_level} or if that doesn't exist, to
  \code{0}. If this argument was specified, the original
  value of \code{av_state$log_level} is be restored at the
  end of \code{var_main}.}

  \item{small}{corresponds to the \code{small} argument of
  Stata's \code{var} function, and defaults to
  \code{FALSE}. This argument affects the outcome of the
  Granger causality test. When \code{small = TRUE}, the
  Granger causality test uses the F-distribution to gauge
  the statistic. When \code{small = FALSE}, the Granger
  causality test uses the Chi-squared distribution to gauge
  the statistic.}

  \item{include_model}{can be used to forcibly include a
  model in the evaluation. Included models have to be
  lists, and can specify the parameters \code{lag},
  \code{exogenous_variables}, and
  \code{apply_log_transform}. For example: \code{ av_state
  <- var_main(av_state,c('Activity_hours','Depression'),
  log_level=3, small=TRUE, include_model=list(lag=3,
  exogenous_variables=data.frame(variable="Depression",
  iteration=1,stringsAsFactors=FALSE),
  apply_log_transform=TRUE))
  var_info(av_state$rejected_models[[1]]$varest) } The
  above example includes a model with \code{lag=3} (so lags
  1, 2, and 3 are included), the model is ran on the
  log-transformed variables, and includes an exogenous
  dummy variable that has a 1 where values of
  \code{log(Depression)} are more than 3.5xstd away from
  the mean (because \code{iteration=1}, see the description
  of the \code{exogenous_max_iterations} parameter above
  for the meaning of the iterations) and 0 everywhere else.
  The included model is added at the start of the list, so
  it can be retrieved (assuming a valid \code{lag} was
  specified) with either
  \code{av_state$accepted_models[[1]]} if the model was
  valid or \code{av_state$rejected_models[[1]]} if it was
  invalid. In the above example, some info about the
  included model is printed (assuming it was invalid).}

  \item{exogenous_variables}{should be a vector of variable
  names that already exist in the given data set, that will
  be supplied to every VAR model as exogenous variables.}

  \item{use_sktest}{affects which test is used for Skewness
  and Kurtosis testing of the residuals. When
  \code{use_sktest = TRUE} (the default), STATA's
  \code{sktest} is used. When \code{use_sktest = FALSE},
  STATA's \code{varnorm} (i.e., the Jarque-Bera test) is
  used.}

  \item{test_all_combinations}{determines whether the
  remaining search space is searched for possible
  additional models. This can sometimes give a few extra
  solutions at a large performance penalty.}

  \item{restrictions.verify_validity_in_every_step}{is an
  argument that affects how constraints are found for valid
  models. When this argument is \code{TRUE} (the default),
  all intermediate models in the iterative
  constraint-finding method have to be valid. This ensures
  that we always find a valid constrained model for every
  valid model. If this argument is \code{FALSE}, then only
  after setting all constraints do we check if the
  resulting model is valid. If this is not the case, we
  fail to find a constrained model.}

  \item{restrictions.extensive_search}{is an argument that
  affects how constraints are found for valid models. When
  this argument is \code{TRUE} (the default), when the term
  with the highest p-value does not provide a model with a
  lower BIC score, we attempt to constrain the term with
  the second highest p-value, and so on. When this argument
  is \code{FALSE}, we only check the term with the highest
  p-value. If restricting this term does not give an
  improvement in BIC score, we stop restricting the model
  entirely.}

  \item{criterion}{is the information criterion used to
  sort the models. Valid options are \code{'AIC'} (the
  default) or \code{'BIC'}.}
}
\value{
  This function returns the modified \code{av_state}
  object. The lists of accepted and rejected models can be
  retrieved through \code{av_state$accepted_models} and
  \code{av_state$rejected_models}. To print these, use
  \code{print_accepted_models(av_state)} and
  \code{print_rejected_models(av_state)}.
}
\description{
  This function generates and tests possible VAR models for
  the specified variables. The only required arguments are
  \code{av_state} and \code{vars}.
}
\examples{
av_state <- load_file("../data/input/Activity and depression pp5 Angela.dta")
av_state <- group_by(av_state,'id')
av_state <- order_by(av_state,'Day')
av_state <- add_derived_column(av_state,'Activity_hours','Activity',
                               operation='MINUTES_TO_HOURS')
av_state <- var_main(av_state,c('Activity_hours','Depression'),log_level=3)
var_info(av_state$accepted_models[[1]]$varest)
}

