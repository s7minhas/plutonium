rm(list=ls())
pth = paste0(here::here(), '/')
source(paste0(pth, 'setup.R'))

# load master dataset
load(paste0(pathIn, "chiData_v2.rda"))

# load dataset that will be merged with the master data
load(paste0(pathIn, "china_aid.rda"))
load(paste0(pathIn, "china_dip.rda"))

names(chiData)

chiData_v3 <- left_join(chiData, china_aid, by = c('cname1', 'cname2', 'year'))
chiData_v3 <- left_join(chiData, chi_dip, by = c('cname1', 'cname2', 'year'))

save(chiData_v3, file = paste0(pathIn, "chiData_v3.rda"))

