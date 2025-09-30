###Visualization stuff
rm(list=ls())

source('setup.R')

#
loadPkg(c(
  'reshape2', 'amen', 'tidyr',
  'ggplot2', 'tidyverse',
  'cshapes', 'countrycode' ))
####

####
load(file=paste0(rpth, 'unMods.rda')) # unMods
####

####
# 

if(!file.exists(paste0(rpth, 'modsForApp.rda'))){

	# 
	reduceSize = function(mod){
		mod = mod[char(2000:max(names(mod)))]
		mod = lapply(mod, function(x){ x[c('U', 'V', 'YPM')] })
		return(mod) }

	# apply fn
	unMods = lapply(unMods, reduceSize)
	####

	####
	# reduce model configs

	# only keep srm variants
	unMods = unMods[grepl('_srm_', names(unMods))]
	####

	####
	#
	save(
		unMods,
		file=paste0(rpth, 'modsForApp.rda') )
}
####
