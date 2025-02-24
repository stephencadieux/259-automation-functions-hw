limit_replace <- function(vec, lower = NULL, upper = NULL) {
  stopifnot(is.numeric(vec))
  stopifnot(sum(!is.na(vec)) > 0)
  if (is.null(lower) | is.null(upper)) {
    bounds <- plus_minus_SD(vec, 1)
    lower <- ifelse(is.null(lower), bounds[1], lower)
    upper <- ifelse(is.null(upper), bounds[2], upper)
  }
  vec <- ifelse(vec < lower | vec > upper, NA, vec)
  return(vec)
}
