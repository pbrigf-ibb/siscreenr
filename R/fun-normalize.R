#' @title normalize screen data
#'
#' @description
#' Normalize data, i.e. express each point as its deviation from a center.
#'
#' @details
#' There are three normalization methods available at the moment:
#' \itemize{
#'   \item{
#'   \code{mean}: subtract reference mean from each data point
#'   }
#'   \item{
#'   \code{median}: subtract reference median from each data point
#'   }
#'   \item{
#'   \code{medpolish}: run Tukey's median polish and return residuals;
#'                     calls \code{stats::medpolish}
#'   }
#' }
#'
#' @param scr screen object, a data frame
#' @param variables variables to normalize;
#'                  character vector of column names or numeric vector of column indices
#' @param group character vector of grouping variables, i.e.
#'              variables whose intersections defines a single group;
#'              passed to \code{dplyr::group_by}
#' @param reference character vector of \code{well_type}s
#'                  to use reference for mean, and median methods;
#'                  passed to \code{dplyr::filter}
#' @param method normalization method, see \code{Details}
#'
#' @return an invisible \code{data.frame}
#'
#' @section Warnings:
#' If you are using the medpolish method, variables will be temporarily converted
#' from vectors to matrices. Make sure your data frames are ordered by column
#' (the default way matrices are filled) rather than by row (the default ScanR format).
#'
#' For other methods a reference subset can be specified.
#' This is done by filtering rows based on the \code{well_type} column, so there must be one.
#' If no reference is declared, normalization will be done against the whole population.
#'

normalize <- function(scr, variables, group, reference,
                      method = c('median', 'mean', 'medpolish')) {
  # check arguments
  missing.columns <- setdiff(variables, names(scr))
  if (length(missing.columns > 0))
    stop('\n',
         'missing variables selected: ', paste(missing.columns, collapse = ', '), '\n',
         'avaiable variables: ', paste(names(scr), collapse = ', '))
  if (missing(reference) & method != 'medpolish')
    message('no reference; data will be normalized to the whole of the population')
  if (!missing(reference) & !is.element('well_type', names(scr)))
      stop('reference specification requires a "well_type" column')
  if (method == 'medpolish' & !missing(reference))
    message('running median polish, "reference" will be ignored')

  # create id column
  scr$temporary_id_column_9000 <- 1:nrow(scr)

  # group data frame
  X <-
    if (missing(group)) scr else
      dplyr::group_by(scr, .dots = group)
  # assign normalization method (methods are defined as separate functions)
  meth <- switch(method,
                 mean = meth.mean,
                 median = meth.median,
                 medpolish = meth.medpolish)
  # do the deed
  if (length(variables) == 1) {
    expr.mut <- parse(text = paste0('meth(', variables, ')'))
    Y <- dplyr::mutate(X, temporary_normalized_variable_name = eval(expr.mut))
    names(Y)[length(names(Y))] <- paste(variables, 'normalized', method, sep = '_')
  } else {
    Y <- dplyr::mutate_at(X, variables, dplyr::funs(normalized = meth))
    names(Y) <- gsub('_normalized$', paste0('_normalized_', method), names(Y))
  }
  # clean up and return
  Z <- Y %>% dplyr::arrange('temporary_id_column_9000') %>% dplyr::select(-'temporary_id_column_9000')
  invisible(Z)
}

meth.mean <- function(x) x - mean(x, na.rm = T)
meth.median <- function(x) x - stats::median(x, na.rm = T)
meth.medpolish <- function(x) {
  if (any(is.infinite(x)))
    stop('infinite values will derail the running median procedure', call. = F)
  X <- get('scr', envir = parent.frame())
  nr <- length(unique(as.character(X$row)))
  nc <- length(unique(as.character(X$column)))
  x_mat <- matrix(x, nrow = nr, ncol = nc)
  polished <- stats::medpolish(x_mat, trace.iter = F, na.rm = T)
  return(as.vector(polished$residuals))
}
