###Visualization stuff
rm(list=ls())
source('setup.R')

#
loadPkg(c(
  'reshape2', 'amen', 'tidyr',
  'ggplot2', 'tidyverse',
  'cshapes', 'countrycode',
  'philentropy'
   ))
####

####
load(paste0(rpth, 'modsForApp.rda')) # unMods
load(paste0(gpth, 'mapCol.rda'))
####

####
# get dist metrics

if(!file.exists(paste0(rpth, 'unDist.rda'))){
	calcDist = function(mods){

	  # params and dist methods
	  distParams = c('U', 'V')
	  distMethods = c('euclidean', 'manhattan', 'cosine')

	  # iterate through mod configs and calc dists
	  distData = lapply(names(mods), function(modLab){

	    # subset mods object
	    modConfig = mods[[modLab]]

	    distDataConfig = lapply(names(modConfig), function(yearLab){

	      distDataParam = lapply(distParams, function(dParam){

	        # subset to relevant param
	        paramMat = mods[[modLab]][[yearLab]][[dParam]]

	        # iterate though distance metrics
	        distDataByMethod = lapply(distMethods, function(dMethod){

	          # relabel id attrs in mats
	          ids = cntryKey$cowc[match(rownames(paramMat), cntryKey$cname)]
	          rownames(paramMat) = ids

	          # get distance calc
	          distMat = distance(paramMat, method=dMethod, use.row.names=TRUE)

	          # org
	          out = reshape2::melt(distMat)
	          out = out[out$Var1 != out$Var2,]
	          out$year = num(yearLab)
	          out$param = dParam
	          out$dist = dMethod
	          out$config = modLab
	          out$lab = with(out, paste0( param, '_', dist ))
	          return(out)

	          }) # close lapply for dist method

	        # org method output
	        distDataByMethod = do.call('rbind', distDataByMethod)
	        return(distDataByMethod)

	        }) # close lapply for param

	      # org param output
	      distDataParam = do.call('rbind', distDataParam)
	      return(distDataParam)

	      }) # close lapply for config

	    # org param output
	    distDataConfig = do.call('rbind', distDataConfig)
	    return(distDataConfig)

	    }) # close lapply for mods

	  #
	  distData = do.call('rbind', distData)
	  return(distData) }


	# apply fn
	unDist = calcDist(unMods)
	####

	####
	save(unDist, file=paste0(rpth, 'unDist.rda'))
}
####

# load(paste0(rpth, 'unDist.rda'))

# dist = unDist[unDist$config=='agree_k2_srm_lfm' & unDist$param=='U',]

# dist = dist[,c('Var1','Var2','year','dist','config','value')]
# dist$configDist = with(dist, paste(config, dist, sep='_'))

# slice = dist[,c('Var1','Var2','year','configDist','value')]

# wide = pivot_wider(slice, names_from=configDist, values_from=value)

# head(wide)

# cor(wide[,4:ncol(wide)])
