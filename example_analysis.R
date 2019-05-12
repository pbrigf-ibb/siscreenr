#library(siscreenr)
library(magrittr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

# go to screen directory
.home <- setwd('C:/Users/Olo/Desktop/R/ground/')
.home <- setwd('C:/Users/Olek/Desktop/R works/ground/')

plot_screen_progress(file = NULL)
layouts('layout_S01_test.txt', 'layout_S01_control.txt', file = 'layout_S01.txt')

s <-
  build_screen('screenlog_S01.txt', 'layout_S01.txt', rem.col = c(0,2), zero.to.NA = T) %>% dplyr::glimpse() %>%
  clean_column_names() %>% dplyr::glimpse() %>%
  dplyr::mutate(position = insert_zeros(position)) %>% dplyr::glimpse()

setwd(.home)

s %<>% mutate(green_fraction = nucs_in_green / (nucs_in_green + nucs_in_red))

vs <- c('nucs_in_green', 'nucs_in_red', 'green_fraction')
vn <- paste(vs, 'normalized_medpolish', sep = '_')
vz <- paste(vn, 'zscore', sep = '_')

n <- s %>% group_by(plate, replica, plated) %>%
  normalize(variables = vs, method = 'medpolish') %>%
  dplyr::glimpse()

R <- s %>% slice(1:max(.$well)) %>% .$well_type %>% grepl('sample', .); matrix(R, 16, 24)

z <- n %>% dplyr::mutate_at(vn, dplyr::funs(zscore = zscore(., reference = R))) %>% dplyr::glimpse()

TRESHOLD <- 2
h <- z %>% dplyr::mutate_at(vz, dplyr::funs(hitscore = hitscore(., TRESHOLD))) %>% dplyr::glimpse()



stop('=== D = O = N = E ===')

microbenchmark::microbenchmark(
  sub = build_screen('screenlog_S01.txt', 'layout_S01.txt', rem.col = c(0,2)),
  conv = build_screen2('screenlog_S01.txt', 'layout_S01.txt', rem.col = c(0,2)),
  times = 10
  )
