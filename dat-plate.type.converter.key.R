#' plate type converter key
#'
#' Create a key table for reading plate type codes and creating
#' corresponding plate type and names and replica numbers.
#'
#' This is a matrix that tells \code{\link{build_screen}} what the type of a plate is
#' based on the 4th field of its file name: \code{001D.20140112.S01.R02}.
#'
#' The key is loaded by \code{\link{plate.type.converter}} during a \code{build_screen} call.
#'
#' If more plate types arise, they should be put here.
#' This makes extending the existing functionalities easy-ish.
#'
#' @format character matrix with three columns:
#' \code{code}, \code{plate_type}, and \cpde{replica}
"plate.type.converter.key"

plate.type.converter.key <- matrix(ncol = 3)
colnames(plate.type.converter.key) <- c('code', 'plate_type', 'replica')
plate.type.converter.key <- rbind(plate.type.converter.key, c('R', 'test', 'rep'))
plate.type.converter.key <- rbind(plate.type.converter.key, c('C', 'control', 'con'))
plate.type.converter.key <- rbind(plate.type.converter.key, c('P', 'positive', 'pos'))
plate.type.converter.key <- rbind(plate.type.converter.key, c('N', 'negative', 'neg'))
plate.type.converter.key <- rbind(plate.type.converter.key, c('A', 'actinonin', 'act'))
#plate.type.converter.key <- rbind(plate.type.converter.key, c('', '', '')

#devtools::use_data(plate.type.converter.key, siscreenr, internal = TRUE)
