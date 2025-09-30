####
rm(list=ls())
source('setup.R')
####

####
load(paste0(dpth, "modData.rda"))
####

####
if(!file.exists(paste0(rpth, 'modelInfoFin.rda'))){
  
# org vars for data explore
levelDVs = c('agree_k2_srm_lfm')

# iv combos
# f1 is about battle deaths in the mideast
# f2 is about troop deployments and military spending
# f3 is about us econ
ivs = c(
  paste0('lag1','_USf', 1:3),
  'lag1_polity', 'lag1_gdp',
  'capdist' )

# org ivs into general specs
ivList = list(
  ivf1 = paste(ivs[-c(2:3)], collapse='+'),
  ivf2 = paste(ivs[-c(1,3)], collapse='+'),
  ivf3 = paste(ivs[-c(1:2)], collapse='+'),
  ivBase = paste(ivs[-(1:3)], collapse='+') )

# re vars
reVars = c( 'cname1', 'polCat3' )
####

####
# org mods to run
modsToRun = expand.grid(
  dv=levelDVs,
  ivs=names(ivList),
  re=reVars,
  type=c('varInt',paste0('varSlopef',1:3)),
  stringsAsFactors=FALSE )

# cleanup
## not running lag dv with delta models
vMods = with(modsToRun, which(ivs!='ivBase' & re=='cname1' & type=='varInt'))
bMods = with(modsToRun, which(ivs=='ivBase' & re=='polCat3' & type!='varInt'))
modsToRun = modsToRun[c(vMods, bMods),]

# add short dv lab
modsToRun$dvLab = unlist(lapply(strsplit(modsToRun$dv, '_'), function(x){x[1]}))
modsToRun = modsToRun[,c('dv','dvLab','ivs','re','type')]
####

####
	save(
	  levelDVs,
	  ivs, ivList, reVars,
	  modsToRun,
	  file=paste0(rpth, 'modelInfoFin.rda'))
}
####
