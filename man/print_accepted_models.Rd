\name{print_accepted_models}
\alias{print_accepted_models}
\title{Print a list of accepted models after a call to var_main}
\usage{
  print_accepted_models(av_state)
}
\arguments{
  \item{av_state}{an object of class \code{av_state} that
  was the result of a call to \code{\link{var_main}}}
}
\description{
  This function prints the list of accepted models when
  \code{av_state} is the result of a call to
  \code{\link{var_main}}. The printed list of models is
  sorted by AIC+BIC score.
}
\examples{
# av_state is the result of a call to var_main
print_accepted_models(av_state)
}

