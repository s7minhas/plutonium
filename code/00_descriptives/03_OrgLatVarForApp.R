###Visualization stuff
rm(list=ls())
pth = paste0(here::here(), '/')
source(paste0(pth, 'setup.R'))

#
loadPkg(c(
  'reshape2', 'amen', 'tidyr',
  'ggplot2', 'tidyverse',
  'cshapes', 'countrycode' ))
####

####
load(file=paste0(pathOut, 'tradeMods.rda')) # tradeMods
load(file=paste0(pathOut, 'unMods.rda')) # unMods
load(file=paste0(pathOut, 'icewsMods.rda')) # icewsMods
####

####
# reduce size of outputs to relevant scope for app

# limit to 2000 forward and reduce output from AME
reduceSize = function(mod){
	mod = mod[char(2000:max(names(mod)))]
	mod = lapply(mod, function(x){ x[c('U', 'V', 'YPM')] })
	return(mod) }

# apply fn
tradeMods = lapply(tradeMods, reduceSize)
unMods = lapply(unMods, reduceSize)
icewsMods = lapply(icewsMods, reduceSize)
####

####
# reduce vars

# trade
# tradeDepSendRaw and and tradeDepSend were very correlated,
# keep only standardized version
tradeMods = tradeMods[!grepl('tradeDepSendRaw_',names(tradeMods))]
# remove tradeGDPRaw as well
tradeMods = tradeMods[!grepl('tradeGDPRaw_',names(tradeMods))]

# icews: only keep gov-gov
icewsMods=icewsMods[grepl('Gov_', names(icewsMods))]
####

####
# reduce model configs

# only keep srm variants
tradeMods = tradeMods[grepl('_srm_', names(tradeMods))]
unMods = unMods[grepl('_srm_', names(unMods))]
icewsMods = icewsMods[grepl('_srm_', names(icewsMods))]
####

####
#
save(
	tradeMods, unMods, icewsMods,
	file=paste0(pathOut, 'modsForApp.rda') )
####
