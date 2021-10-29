###Visualization stuff
rm(list=ls())
pth = paste0(here::here(), '/')
source(paste0(pth, 'setup.R'))

#
loadPkg(c(
  'reshape2', 'amen', 'tidyr',
  'ggplot2', 'tidyverse',
  'cshapes', 'countrycode',
  'philentropy'
   ))
####

####
load(paste0(pathOut, 'modsForApp.rda')) # unMods, tradeMods, icewsMods
load(paste0(pathGraphics, 'mapCol.rda'))
####

####
# get dist metrics
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
tradeDist = calcDist(tradeMods)
icewsDist = calcDist(icewsMods)
####

####
save(unDist, file=paste0(pathOut, 'unDist.rda'))
save(tradeDist, file=paste0(pathOut, 'tradeDist.rda'))
save(icewsDist, file=paste0(pathOut, 'icewsDist.rda'))
####

####

head(unDist)

unique(unDist$config)

unSlice = unDist[unDist$config %in% c('agree_k2_srm_lfm','agree_k5_srm_lfm', 'agree_k8_srm_lfm'),]
unSlice = unSlice[unSlice$dist!='euclidean',]

library(tidyr)

unWide = pivot_wider(unSlice, names_from = config, values_from=value)

cor(unWide[unWide$dist=='cosine', c('agree_k2_srm_lfm','agree_k5_srm_lfm', 'agree_k8_srm_lfm')])

unique(tradeDist$config)

tconfigs = unique(tradeDist$config)[grepl('tradeDep',unique(tradeDist$config))]

tradeSlice = tradeDist[tradeDist$config %in% tconfigs,]
tradeSlice = tradeSlice[tradeSlice$dist!='euclidean',]

tradeWide = pivot_wider(tradeSlice, names_from = config, values_from=value)

cor(tradeWide[tradeWide$dist=='cosine', tconfigs])


## k5 k2
## tradeDepSend agree
####
