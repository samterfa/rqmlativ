
source('R/zzz.R')
skyObjects <- loadSkyObjects()

usethis::use_data(skyObjects, overwrite = TRUE)