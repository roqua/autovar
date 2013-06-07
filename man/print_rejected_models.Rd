\name{print_rejected_models}
\alias{print_rejected_models}
\title{Print a list of rejected models after a call to var_main}
\usage{
  print_rejected_models(av_state)
}
\arguments{
  \item{av_state}{an object of class \code{av_state} that
  was the result of a call to \code{\link{var_main}}}
}
\description{
  This function prints the list of rejected models when
  \code{av_state} is the result of a call to
  \code{\link{var_main}}.
}
\examples{
av_state <- load_file("../data/input/pp5 nieuw compleet.sav",log_level=3)
av_state <- var_main(av_state,c('SomBewegUur','SomPHQ'),criterion="BIC",log_level=3)
# av_state is the result of a call to var_main
print_rejected_models(av_state)
}

