\name{print_best_models}
\alias{print_best_models}
\title{Prints the best model from the list of accepted models}
\usage{
  print_best_models(av_state)
}
\arguments{
  \item{av_state}{an object of class \code{av_state} that
  was the result of a call to \code{\link{var_main}}}
}
\description{
  This functions uses log transformation (logm).If logm is
  true it gives the best log-transformed model. And if the
  logm is false it gives the best model without
  log-transform. The best model is the measured with the
  lowest AIC+BIC value.
}
\examples{
print_best_model(av_state)
}

