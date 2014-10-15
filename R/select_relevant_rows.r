#' Select and return the relevant rows
#'
#' This function returns the data frame with the beginning and trailing rows that are all NA removed, it also returns updated \code{first_measurement_index} and \code{timestamp} variables. The timestamp may advance as a result of the data frame starting with three or more NA's.
#' @param data a data frame
#' @param timestamp the date of the first measurement in the format \code{'yyyy-mm-dd'}
#' @param net_cfg a net_cfg object providing metadata about the networks
#' @return This function returns a list with three elements, \code{data} is the modified data frame, \code{first_measurement_index} is the possibly adjusted first_measurement index, and \code{timestamp} is the possibly updated timestamp (day).
#' @examples
#' data <- data.frame(id=c(NA,NA,NA,1,1,NA),tijdstip=c(NA,NA,NA,3,4,NA),something=c(NA,NA,NA,9,16,NA))
#' data
#' net_cfg <- new_net_cfg()
#' net_cfg$measurements_per_day <- 2
#' select_relevant_rows(data,'2014-05-06',net_cfg)
#' @export
select_relevant_rows <- function(data,timestamp,net_cfg) {
  mask <- rep(TRUE,nr_rows(data))
  meas_per_day <- net_cfg$measurements_per_day
  firstvalidrow <- 0
  for (i in 1:nr_rows(data))
    if (firstvalidrow == 0 && all(is.na(data[i,]))) {
      mask[i] <- FALSE
    } else if (firstvalidrow == 0) {
      firstvalidrow <- i
    }
  first_meas_idx <- 1+((firstvalidrow+meas_per_day-1)%%meas_per_day)
  days_to_advance <- (firstvalidrow-1)%/%meas_per_day
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
