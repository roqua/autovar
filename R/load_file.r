# loads the file and saves the current directory
load_file <- function(filename,file_type = c('SPSS','STATA'),log_level=0) {
  av_state <- new_av_state()
  real_file_name <- filename
  if (!is.null(attr(filename,"filename"))) {
    real_file_name <- attr(filename,"filename")
  }
  if (missing(file_type)) {
    file_type <- determine_file_type(real_file_name)
  }
  working_dir <- paste(getwd(),'/',sep="")
  file_name <- paste(working_dir,filename,sep="")
  if (!file.exists(file_name)) {
    if (!file.exists(filename)) {
      stop(paste("File does not exist:",filename))
    }
    # NOTE: filename needs to include the full and valid path for SPSS export to work
    # if the data is stored in a separate file
    file_name <- filename
  }
  av_state$file_name <- file_name
  av_state$real_file_name <- real_file_name
  av_state$file_type <- match.arg(file_type)

  av_state <- switch(av_state$file_type,
    SPSS = load_file_spss(av_state),
    STATA = load_file_stata(av_state)
  )
  scat(log_level,2,"load_file loaded",av_state$file_type,"file with",length(av_state$data[[1]]),"columns:\n")
  sprint(log_level,2,column_names_output(av_state))
  av_state
}

column_names_output <- function(av_state) {
  dnames <- NULL
  for (name in names(av_state$data[[1]])) {
    column_type <- decorate_class(class(av_state$data[[1]][[name]]))
    dnames <- c(dnames,paste(name," (",column_type,")",sep=""))
  }
  dnames
}

decorate_class <- function(classname) {
  switch(classname,
    factor = "nom",
    numeric = "scl"
  )
}

determine_file_type <- function(filename) {
  fileonly <- basename(filename)
  if (length(grep("\\.dta$",fileonly)) == 1) {
    'STATA'
  } else if (length(grep("\\.sav$",fileonly)) == 1) {
    'SPSS'
  } else {
    warning(paste("Unrecognized file type:",filename))
  }
}

load_file_spss <- function(av_state) {
  av_state$raw_data <- read_spss(av_state$file_name, to.data.frame=TRUE)
  av_state$data <- list(multiple=av_state$raw_data)
  av_state
}

load_file_stata <- function(av_state) {
  av_state$raw_data <- read.dta(av_state$file_name)
  av_state$data <- list(multiple=av_state$raw_data)
  av_state
}
