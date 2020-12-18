
source('R/zzz.R')

skyFields <- loadSkyFields()

# Loading in code instead
usethis::use_data(skyFields, overwrite = T, internal = F)
