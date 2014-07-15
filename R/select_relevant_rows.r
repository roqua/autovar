#' Select and return the relevant rows
#' 
#' This function returns the data frame with the beginning and trailing rows that are all NA removed, it also returns updated \code{first_measurement_index} and \code{timestamp} variables. The timestamp may advance as a result of the data frame starting with three or more NA's.
#' @param data a data frame
#' @param timestamp the date of the first measurement in the format \code{'yyyy-mm-dd'}
#' @return This function returns a list with three elements, \code{data} is the modified data frame, \code{first_measurement_index} is the possibly adjusted first_measurement index, and \code{timestamp} is the possibly updated timestamp (day).
#' @export
select_relevant_rows <- function(data,timestamp) {
  mask <- rep(TRUE,nr_rows(data))
  firstvalidrow <- 0
  for (i in 1:nr_rows(data))
    if (firstvalidrow == 0 && all(is.na(data[i,]))) {
      mask[i] <- FALSE
    } else if (firstvalidrow == 0) {
      firstvalidrow <- i
    }
  first_meas_idx <- 1+((firstvalidrow+2)%%3)
  days_to_advance <- (firstvalidrow-1)%/%3
  if (days_to_advance > 0)
    timestamp <- as.character(timeSequence(from=timeDate(as.Date(timeDate(timestamp))),
                                           length.out=days_to_advance+1,by="day")[days_to_advance+1])
  lastvalidrow <- 0
  for (i in nr_rows(data):1)
    if (lastvalidrow == 0 && all(is.na(data[i,]))) {
      mask[i] <- FALSE
    } else if (lastvalidrow == 0) {
      lastvalidrow <- i
    }
  list(data=data[mask,],first_measurement_index=first_meas_idx,timestamp=timestamp)
}
