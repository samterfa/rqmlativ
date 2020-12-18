
source('R/zzz.R')

skyObjects <- loadSkyObjects()

# Loading in code instead
usethis::use_data(skyObjects, overwrite = T, internal = F)
