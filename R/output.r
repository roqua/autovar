# redirect methods for print and cat

# levels used for printing
# 0 = debug
# 1 = test detail
# 2 = test outcomes
# 3 = normal

sprint <- function(level,...) {
  if (passes_log_level(level)) {
    print(...)
  }
}

scat <- function(level,...) {
  if (passes_log_level(level)) {
    cat(...)
  }
}

passes_log_level <- function(level) {
  (is.null(av_state$log_level) || level >= av_state$log_level)
}
