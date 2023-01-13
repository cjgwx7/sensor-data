find_high_mortality <- function(x, threshold) {

  MEDIAN <- median(x, na.rm = TRUE) # nolint
  MAD <- mad(x, na.rm = TRUE) # nolint
  STATISTIC <- (x - MEDIAN) / MAD # nolint
  THRESHOLD <- ifelse(STATISTIC >= threshold, 1, 0) # nolint
  return(THRESHOLD)

}