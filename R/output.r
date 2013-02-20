# redirect methods for print and cat

# levels used for printing
# 0 = debug
# 1 = test detail
# 2 = test outcomes
# 3 = normal

sprint <- function(log_level,level,...) {
  if (passes_log_level(log_level,level)) {
    print(...)
  }
}

scat <- function(log_level,level,...) {
  if (passes_log_level(log_level,level)) {
    cat(...)
  }
}

passes_log_level <- function(log_level,level) {
  (is.null(log_level) || level >= log_level)
}
