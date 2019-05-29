
library(magrittr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(data.table)
library(acutils)
library(siscreenr)

# go to screen directory
.home <- setwd('C:/Users/Olo/Desktop/R/ground/')
.home <- setwd('C:/Users/Olek/Desktop/R works/ground/')

ann <- data.table::fread('ANNOTATION.LIBRARY.GENOMIC_20181108.txt')

siscreenr::plot_screen_progress('C:/Users/Olek/Desktop/R works/ground/', file = NULL)
#layouts('layout_S01_test.txt', 'layout_S01_control.txt', file = 'layout_S01.txt')

s <-
  build_screen(logfile = 'C:/Users/Olek/Desktop/R works/ground/screenlog_S01.txt',
               layout = 'C:/Users/Olek/Desktop/R works/ground/layout_S01.txt',
               datadir = 'C:/Users/Olek/Desktop/R works/ground/data/',
               rem.col = c(0,2), zero.to.NA = T) %>%
  acutils::clean_column_names() %>%
  dplyr::mutate(position = insert_zeros(position))

setwd(.home)

s %<>% dplyr::mutate(green_fraction = nucs_in_green / (nucs_in_green + nucs_in_red))

vs <- c('nucs_in_green', 'nucs_in_red', 'green_fraction')
vn <- paste(vs, 'normalized_medpolish', sep = '_')
vz <- paste(vn, 'zscore', sep = '_')
vh <- paste(vz, 'hitscore', sep = '_')

n <- s %>% dplyr::group_by(plate, replica, plated) %>%
  normalize(variables = vs, method = 'medpolish')

z <- n %>% dplyr::group_by(plate_type) %>%
  siscreenr::zscore(dev = T, ref = well_type == 'sample', var = vs)

stop('just stop')

TRESHOLD <- 2
h <- z %>% dplyr::ungroup() %>%
  dplyr::mutate_at(.vars = vz, .funs = list(hitscore = ~hitscore(., TRESHOLD))) %>% dplyr::glimpse()

r <- h %>% dplyr::group_by(plate, plated, imaged, well) %>%
  dplyr::mutate_at(vh, .funs = list(sumhitscore = sum)) %>% dplyr::glimpse()

r <- h %>% dplyr::group_by(plate, plated, imaged, well) %>%
  dplyr::mutate_at(vh, .funs = list(
    frachitscore = function(x) {sum(x, na.rm = T) / length(x[!is.na(x)])})) %>% dplyr::glimpse()


stop('=== D = O = N = E ===')
