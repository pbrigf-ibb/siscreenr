#' @title siscreenr: High Throughput Data Analysis
#'
#' @description
#' A package for analyzing high throughput microscopy data produced by ScanR systems.
#' Provides tools for loading, normalizing and scoring data, and hit selection.
#' It is intended for interactive use so most of the functions leave a considerable amount of work to teh user.
#'
#' @details
#' The data is obtained with an automated fluorescence microscope and
#' preliminary image analysis is done with ScanR software.
#' Most experiments are done in 384 well plate format and usually involve dozens of plates.
#' However, it is compatible with other plate formats and some concepts can be transferred to slides as well.
#'
#' @author Aleksander Chlebowski
#'
#' @section Functions:
#' \code{build_screen} collate all data into a single data frame, reaarrange and add layout
#'
#' \code{normalize} normalize data by one of the available methods: subtract mean/median of reference or Tukey's median polish (for matrices)
#'
#' \code{zscore} calculate (robust) z scores
#'
#' \code{hit.score} classify data point as hit or not based on a given treshold; usually used on z scores
#'
#' \code{sum.score} summarize hit scores, either as a sum or as a fraction and assign hit status to data point
#'
#' @docType package
#' @name siscreenr
NULL
