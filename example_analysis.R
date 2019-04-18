
s <-
  build_screen() %>%
  clean_column_names() %>%
  separate_flag('wells_rescanned', newname = 'rescanned')
