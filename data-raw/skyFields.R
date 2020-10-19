
source('R/zzz.R')
skyFields <- loadSkyFields()

usethis::use_data(skyFields, overwrite = TRUE)