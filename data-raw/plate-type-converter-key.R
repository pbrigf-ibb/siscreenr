plate.type.converter.key <- matrix(ncol = 3)
colnames(plate.type.converter.key) <- c('code', 'plate_type', 'replica')
plate.type.converter.key <- rbind(plate.type.converter.key, c('R', 'test', 'rep'))
plate.type.converter.key <- rbind(plate.type.converter.key, c('C', 'control', 'con'))
plate.type.converter.key <- rbind(plate.type.converter.key, c('P', 'positive', 'pos'))
plate.type.converter.key <- rbind(plate.type.converter.key, c('N', 'negative', 'neg'))
plate.type.converter.key <- rbind(plate.type.converter.key, c('A', 'actinonin', 'act'))
#plate.type.converter.key <- rbind(plate.type.converter.key, c('', '', '')

plate.type.converter.key <- as.data.frame(plate.type.converter.key, stringsAsFactors = FALSE)

#usethis::use_data(plate.type.converter.key, internal = TRUE)
