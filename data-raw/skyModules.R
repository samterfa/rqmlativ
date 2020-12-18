
source('R/zzz.R')

skyModules <- loadSkyModules()

# Loading in code instead
usethis::use_data(skyModules, overwrite = T, internal = F)