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
  build_screen('screenlog_S01.txt', 'layout_S01.txt', rem.col = c(0,2)) %>% dplyr::glimpse() %>%
  clean_column_names() %>% dplyr::glimpse() %>%
  dplyr::mutate(position = insert_zeros(position)) %>%  dplyr::glimpse()

setwd(.home)

s %<>% mutate(green_fraction = nucs_in_green / (nucs_in_green + nucs_in_red))

n <- normalize(s, variables = c('nucs_in_green', 'nucs_in_red', 'green_fraction'),
               group = c('plate', 'replica', 'plated'), reference = 'nt', method = 'medpolish')



stop('=== D = O = N = E ===')

microbenchmark::microbenchmark(
  sub = build_screen('screenlog_S01.txt', 'layout_S01.txt', rem.col = c(0,2)),
  conv = build_screen2('screenlog_S01.txt', 'layout_S01.txt', rem.col = c(0,2)),
  times = 10
  )
