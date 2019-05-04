library(siscreenr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

# go to screen directory
setwd('C:/Users/Olo/Desktop/R/ground/')

plot_screen_progress(file = NULL)
layouts('layout_S01_test.txt', 'layout_S01_control.txt', file = 'layout_S01.txt')

s <-
  build_screen('screenlog_S01.txt', 'layout_S01.txt', rem.col = c(0,2)) %>% dplyr::glimpse() %>%
  clean_column_names() %>% dplyr::glimpse() %>%
  dplyr::mutate(position = insert_zeros(position)) %>%  dplyr::glimpse()

microbenchmark(A = build_screen('screenlog_S01.txt', 'layout_S01.txt', rem.col = c(0,2)), times = 10)
