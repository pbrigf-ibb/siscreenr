#' detect and reformat plate type and replica designation
#'
#' This is in internal function called by \code{build_screen}.
#'
#' @param x a screen object
#' @param key an external dictionary, defaults to plate.type.converter.key,
#'            a character matrix that should be attached with the package
#'
#' @return screen object in which columns "plate_type" and "replica" are altered
#'
#' @importFrom magrittr %>%
#'



# construct key:
plate.type.converter.key <- matrix(ncol = 3)
colnames(plate.type.converter.key) <- c('code', 'plate_type', 'replica')
plate.type.converter.key <- rbind(plate.type.converter.key, c('R', 'test', 'rep'))
plate.type.converter.key <- rbind(plate.type.converter.key, c('C', 'control', 'con'))
plate.type.converter.key <- rbind(plate.type.converter.key, c('P', 'positive', 'pos'))
plate.type.converter.key <- rbind(plate.type.converter.key, c('N', 'negative', 'neg'))
plate.type.converter.key <- rbind(plate.type.converter.key, c('A', 'actinonin', 'act'))
plate.type.converter.key <- as.data.frame(plate.type.converter.key, stringsAsFactors = FALSE)

plate.type.converter <- function(x, key = plate.type.converter.key) {
  dots.replica <- c(setNames(as.list(key$replica), key$code), 'unknown')
  dots.type <- c(setNames(as.list(key$plate_type), key$code), 'unknown')
  f_replica <- function(x) do.call(switch, c(x, dots.replica))
  f_plate_type <- function(x) do.call(switch, c(x, dots.type))

  x$replica <- vapply(x$plate_type, f_replica, character(1))
  x$plate_type <- vapply(x$plate_type, f_plate_type, character(1))

  return(x)
}


# benchmarking
# a <- rep(c('R', 'C'), 10)
# f_sub <- function(x) {
#   x <- gsub('R', 'rep', x)
#   x <- gsub('C', 'con', x)
#   x <- gsub('P', 'pos', x)
#   x <- gsub('N', 'neg', x)
#   x <- gsub('A', 'act', x)
#   return(x)
# }
# f_swi <- function(x) {
#   f <- function(x) {
#     switch(x,
#            'R' = 'rep',
#            'C' = 'con',
#            'P' = 'pos',
#            'N' = 'neg',
#            'A' = 'act')
#   }
#   vapply(x, f, character(1), USE.NAMES = FALSE)
# }
# microbenchmark::microbenchmark(f_sub(a), f_swi(a))


