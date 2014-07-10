opencpu_wrapper <- function(x,myfilenamearg) {
  myfilename <<- myfilenamearg
  eval(parse(text=x))
}
