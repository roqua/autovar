\name{var_info}
\alias{var_info}
\title{Print summary information and tests for a VAR model estimation}
\usage{
  var_info(varest, log_level = 0)
}
\arguments{
  \item{varest}{an object of class \code{varest}}

  \item{log_level}{sets the minimum level of output that
  should be shown (a number between 0 and 3). A lower level
  means more verbosity.}
}
\description{
  This function prints a summary and the output of the
  tests for a VAR model. The tests it shows are the
  Eigenvalue stability condition, the Portmanteau tests,
  the Jarque-Bera tests, the sk tests, the Granger
  causality Wald tests, and Stata's \code{estat ic}.
}
\examples{
# av_state is the result of an earlier call to var_main
var_info(av_state$accepted_models[[1]]$varest)
var_info(av_state$rejected_models[[1]]$varest)
}

