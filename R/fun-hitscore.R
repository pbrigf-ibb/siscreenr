#' assign hit scoes
#'
#' Assign hit status to single wells in a screen based on their z scores.
#'
#' A data point is considered a hit if its hitting variable (usually a z score)
#' exceeds a treshold. A point can have one of three hitscores:
#' \itemize{
#' \item{
#' 1 (positive hit) if its value is equal to or higher than the treshold
#' }
#' \item{
#' -1 (negative hit) if its value is equal or lowerto  than the negative treshold
#' }
#' \item{
#' 0 (non-hit) if its absolute value is lower than the treshold
#' }
#' }
#'
#' @param x a numeric vector
#' @param treshold absolute treshold for hit status
#'
#' @return a vector of hit statuses represented as integers: 1, -1 or 0

hitscore <- function(x, treshold = 2) {
  if (!is.nuemric(x)) stop('"x" must be numeric')
  # x1 <- ifelse(x > -treshold & x < treshold, 0L, x)
  x1 <- ifelse(abs(x) < treshold, 0L, x)
  x2 <- ifelse(x1 <= -treshold, -1L, x1)
  x3 <- ifelse(x2 >= treshold, 1L, x2)
  return(x3)
}
