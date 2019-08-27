#' draw a screen overview as a barplot
#'
#' Produces a barplot of mean z scores of the green fraction of sample wells, from lowest to highest.
#' The function is designed to highlight a group of genes (e.g. components of a complex)
#' but can be made to work with single genes.
#'
#' Mean green fraction z scores of all sample wells will be drawn as grey bars.
#' A set of genes specified by the \code{highlight} argument will be highlighted
#' in \code{orangered4} (the bars will be slightly widened for clarity).
#' Highlighting is specified by a logical column that flags the proper genes.
#' This column must be put in \code{data} beforehand.
#'
#' @param data a \code{data.frame}; must contain the column "mean_zscore_gf"
#' @param highlight regular expression (case insensitive) that matches the name of a column
#'                  that contains a logical flag for the group of genes to highlight
#' @param treshold integer; z* score treshold used for hit selection; location of horizontal lines
#'
#' @return a ggplot object
#'
#' @export
#'
drawmeabarplot <- function(data, highlight, treshold = 2) {
  #check arguments
  if (!is.element('mean_zscore_gf', names(data))) stop('"data" must contain "mean_zscore_gf"')
  if (!is.character(highlight) | length(highlight) != 1) stop('"x" must be a character string')
  HIGHLIGHT <- toupper(grep(highlight, names(data), ignore.case = T, value = T))
  if (length(HIGHLIGHT) == 0) stop('\"highlight\" does not match any columns')
  if (length(HIGHLIGHT) > 1) stop('\"highlight\" matches more than one column')

  # prepare data
  data <- data[order(data$mean_zscore_gf), ]
  data$ID <- 1:nrow(data)
  filtered <- data[data[[HIGHLIGHT]], ]
  # prepare plot title; some reformatting for specific cases
  title <-
    if (HIGHLIGHT == 'SPLICING') {"Genes involved in mRNA splicing"
    } else if (HIGHLIGHT == 'POLYADENYLATION') {"Genes involved in mRNA polyadenylation"
    } else {
      if (HIGHLIGHT %in% c('MEDIATOR', 'EXOSOME')) {
        HIGHLIGHT <- paste0(substr(HIGHLIGHT, 1, 1),
                            tolower(substr(HIGHLIGHT, 2, nchar(HIGHLIGHT))))
      }
      paste(HIGHLIGHT, 'complex genes')
    }
  # produce plot
  ggplot2::ggplot(data, ggplot2::aes(x = ID, y = mean_zscore_gf)) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.line.x = ggplot2::element_blank()) +
    ggplot2::xlab('') +ggplot2::ylab('mean green fraction z* score') +
    ggplot2::ggtitle(title) +
    ggplot2::geom_col(fill = 'grey75') +
    ggplot2::geom_col(data = filtered, fill = 'orangered4', width = 25) +
    ggplot2::geom_hline(yintercept = c(-treshold, treshold), lty = 2, size = 0.5) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   plot.subtitle = ggplot2::element_text(hjust = 0.5))
}
