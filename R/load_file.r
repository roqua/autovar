# load_file

# loads the file and saves the current directory
load_file <- function(filename,file_type = c('SPSS','STATA')) {
  if (missing(file_type)) {
    file_type <- determine_file_type(filename)
  }
  working_dir <- paste(gsub('\\\\','/',system('pwd',intern=TRUE)),'/',sep="")
  file_name <- paste(working_dir,filename,sep="")
  if (!file.exists(file_name)) {
    if (!file.exists(filename)) {
      halt(paste("File does not exist:",filename))
    }
    # NOTE: filename needs to include the full and valid path for SPSS export to work.
    file_name <- filename
  }
  av_state$file_name <<- file_name
  av_state$file_type <<- match.arg(file_type)

  switch(av_state$file_type,
    SPSS = load_file_spss(av_state$file_name),
    STATA = load_file_stata(av_state$file_name)
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

load_file_spss <- function(filename) {
  cat("load_file_spss",filename,"\n")
  av_state$raw_data <<- read_spss(filename, to.data.frame=TRUE)
  av_state$data <<- list(multiple=av_state$raw_data)
}

load_file_stata <- function(filename) {
  cat("load_file_stata",filename,"\n")
}
