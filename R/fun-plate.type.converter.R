#' detect and reformat plate type and replica designation
#'
#' This is in internal function called by \code{build_screen}.
#'
#' This work is in progress but nears completion. Benchmarked but not tested.
#'
#' @param x a screen object
#' @param key an external dictionary, defaults to plate.type.converter.key
#'
#' @return character vector changed according to an external dictionary
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

plate.type.converter <- function(x, key = plate.type.converter.key) {
  dots.type <- key[, c('code', 'plate_type')] %>% apply(1, paste, collapse = ' = ')
  dots.replica <- key[, c('code', 'replica')] %>% apply(1, paste, collapse = ' = ')
  f_plate_type <- function(x) switch(x, dots.type)
  f_replica <- function(x) switch(x, dots.replica)

  x$plate_type <- vapply(x$plate_type, f_plate_type, character(1))
  x$replica <- vapply(x$replica, f_replica, character(1))

  return(x)
}


# benchmarking
# a <- rep(c('R', 'C'), 10)
# f_sub <- function() {
#   a <- gsub('R', 'rep', a)
#   a <- gsub('C', 'con', a)
#   a <- gsub('P', 'pos', a)
#   a <- gsub('N', 'neg', a)
#   a <- gsub('A', 'sct', a)
# }
# f_swi <- function() {
#   f <- function(x) {
#     switch(x,
#            'R' = 'rep',
#            'C' = 'con',
#            'P' = 'pos',
#            'N' = 'neg',
#            'A' = 'act')
#   }
#   vapply(a, f, character(1))
# }
# microbenchmark::microbenchmark(f_sub(), f_swi())
