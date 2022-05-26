mad_thresholding <- function(x, thresh, constant) {

  median_x <- median(x, na.rm = TRUE)
  abs_dev <- abs(x - median_x)
  median_abs_dev <- median(abs_dev, na.rm = TRUE)
  threshold <- median_x + thresh * (constant * median_abs_dev)
  out <- ifelse(x > threshold, 1, 0)
  return(out)

}