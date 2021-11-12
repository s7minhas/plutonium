####
rm(list=ls())
pth = paste0(here::here(), '/')

#
source(paste0(pth, 'setup.R'))

#
loadPkg(c('lme4', 'doParallel', 'foreach'))
####

####
load(paste0(pathIn, "modData.rda"))
####

####
# org vars for data explore
deltaDVs = names(modData)[grepl('delta_',names(modData))]
levelDVs = gsub('delta_','',deltaDVs)

# lagged dvs
lagDVs = paste0('lag1_', levelDVs)

# iv combos
ivs = c(
  'lag1_USf1', 'lag1_USf2',
  'lag1_polity', 'lag1_gdp',
  'capdist' )

# org ivs into general specs
ivList = list(
  ivAll = paste(ivs, collapse='+'),
  ivf1 = paste(ivs[-2], collapse='+'),
  ivf2 = paste(ivs[-1], collapse='+'),
  ivBase = paste(ivs[-(1:2)], collapse='+') )

# re vars
reVars = c(
  'cname1',
  'region23', 'region', 'chinaRegions',
  'polCat3', 'polCatLo', 'polCatHi',
  'minChinaCatLo', 'minChinaCatHi',
  'metalsChinaCatLo', 'metalsChinaCatHi' )
####

####
# org mods to run
modsToRun = expand.grid(
  dv=c(deltaDVs, levelDVs),
  ivs=names(ivList),
  re=reVars,
  type=c('varInt','varSlope_f1','varSlope_f2'),
  lagDV=c(TRUE,FALSE), stringsAsFactors=FALSE )

# cleanup
## not running lag dv with delta models
modsToRun = modsToRun[!(grepl('delta_',modsToRun$dv) & modsToRun$lagDV==TRUE),]

## not using usf1 as iv when varying by slope
modsToRun = modsToRun[!(modsToRun$type=='varSlope_f2' & modsToRun$ivs %in% c('ivf1','ivf2','ivAll')),]

## not using usf2 as iv when varying by slope
modsToRun = modsToRun[!(modsToRun$type=='varSlope_f1' & modsToRun$ivs %in% c('ivf1','ivf2','ivAll')),]
####

####
save(
  deltaDVs, levelDVs, lagDVs,
  ivs, ivList, reVars,
  modsToRun,
  file=paste0(pathOut, 'modelInfo.rda'))
####
