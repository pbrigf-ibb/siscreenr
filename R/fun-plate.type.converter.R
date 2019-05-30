#' reformat plate type and replica designation
#'
#' This is an internal function called by \code{build_screen}.
#' It checks the plate type encoded in field 4 of the plate name and changes the columns
#' \code{plate_type} and \code{replica}, according to a dictionary.
#'
#' The dictionary is supplied as a data frame, by default one found in a file
#' in the package's \code{extdata} directory. The file contains five options as of May 2019
#' and more options can easily be added by the user.
#'
#' Alternatively, the dictionary can be taken from a custom file or an object specified by
#' the \code{key} argument. Be careful to keep to the format of the default file.
#'
#' To view or edit the dictionary file find it with \code{path.package('siscreenr')}.
#'
#' @param x a screen object
#' @param key optional dictinoary file or object, see \code{Details}
#'
#' @return screen object in which columns "plate_type" and "replica" are altered
#'
#' @keywords internal
#'

plate.type.converter <- function(x, key) {
  if (missing(key)) key <- paste0(path.package('siscreenr'), '/extdata/plate.type.converter.key.txt')
  if (is.character(key)) key <- utils::read.delim(key, stringsAsFactors = FALSE)
  if (!is.data.frame(key)) stop('"key" must be a data frame or a path to a file containing one')
  if (any(sapply(key, is.factor))) {
    key <- cbind(
      Filter(Negate(is.factor), key),
      as.data.frame(lapply(Filter(Negate(is.factor), key), factor))
    )
  }

  dots.replica <- c(stats::setNames(as.list(key$replica), key$code), 'unknown')
  dots.type <- c(stats::setNames(as.list(key$plate_type), key$code), 'unknown')
  f_replica <- function(x) do.call(switch, c(x, dots.replica))
  f_plate_type <- function(x) do.call(switch, c(x, dots.type))

  x$replica <- vapply(x$plate_type, f_replica, character(1))
  x$plate_type <- vapply(x$plate_type, f_plate_type, character(1))

  return(x)
}

#' @examples
#' d <- data.frame(plate_type = rep(c('R', 'C'), each = 3))
#' plate.type.converter(d)

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

