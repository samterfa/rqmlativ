
source('R/zzz.R')
skyRelationships <- loadSkyRelationships()

usethis::use_data(skyRelationships, overwrite = TRUE)