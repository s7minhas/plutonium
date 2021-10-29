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
load(file=paste0(pathOut, 'tradeMods.rda')) # tradeMods
load(file=paste0(pathOut, 'unMods.rda')) # unMods
load(file=paste0(pathOut, 'icewsMods.rda')) # icewsMods
load(paste0(pathGraphics, 'mapCol.rda'))
####

####
distParams = c('U', 'V', 'UVPM')
distMethods = philentropy::getDistMethods()
distMethods = c('euclidean', 'manhattan', 'cosine')

mods = tradeMods
# modLab = names(mods)[1]
# modConfig = mods[[modLab]]
# yearLab = names(modConfig)[1]
# dParam = distParams[3]
# dMethod = distMethods[1]

distData = lapply(names(mods), function(modLab){

  # subset mods object
  modConfig = mods[[modLab]]

  distDataConfig = lapply(names(modConfig), function(yearLab){

    distDataParam = lapply(distParams, function(dParam){

      # subset to relevant param
      paramMat = mods[[modLab]][[yearLab]][[dParam]]

      # iterate though distance metrics
      distDataByMethod = lapply(distMethods, function(dMethod){

        # for U or V we need to use dist method
        if(dParam %in% c('U', 'V') ){

          # relabel id attrs in mats
          ids = cntryKey$cowc[match(rownames(paramMat), cntryKey$cname)]
          rownames(paramMat) = ids

          # get distance calc
          distMat = distance(paramMat, method=dMethod, use.row.names=TRUE) }

        # for UVPM we can just melt down
        if(dParam == 'UVPM'){
          # relabel id attrs in mats
          ids = cntryKey$cowc[match(rownames(paramMat), cntryKey$cname)]
          rownames(paramMat) = colnames(paramMat) = ids
          distMat = paramMat }

          # org
          out = reshape2::melt(distMat)
          out = out[out$Var1 != out$Var2,]
          out$year = num(yearLab)
          out$param = dParam
          if(dParam %in% c('U', 'V')){
            out$dist = dMethod
          } else { out$dist = NA }
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

distData = unique(do.call('rbind', distData))
####


# distance metrics


# iterate through time
distData = lapply(1:length(mods), function(iiMod){

  })

distDataConfig = lapply(1:length(modConfig), function(tt){

  # iterate through parameters
  distDataParam = lapply(1:numParams, function(ii){

    # org params
    listParams = list()
    if(paramToPlotForDist=='U'){
      listParams[[1]] = modConfig[[tt]]$'U'
      names(listParams) = 'U'}
    if(paramToPlotForDist=='V'){
      listParams[[1]] = modConfig[[tt]]$'V'
      names(listParams) = 'V'}
    if(paramToPlotForDist=='U and V'){
      listParams[[1]] = modConfig[[tt]]$'U'
      listParams[[2]] = modConfig[[tt]]$'V'
      names(listParams) = c('U', 'V') }

    # subset to relev param
    paramMat = listParams[[ii]]

    # iterate through distance metrics
    distDataByMethod = lapply(distToPlot, function(distMethod){

      # relabel id attrs in mats
      ids=cntryKey$cowc[match(rownames(paramMat), cntryKey$cname)]
      rownames(paramMat) = ids

      # get distance calc
      distMat = distance(paramMat, method=distMethod, use.row.names=TRUE)

      # org
      out = reshape2::melt(distMat)
      out$year = num(names(modConfig)[tt])
      out$param = names(listParams)[ii]
      out$dist = distMethod
      out$config = names(mods)[iiMod]
      out$lab = with(out, paste0( param, '_', dist ))
      return(out) })

    # org
    distDataByMethod = do.call('rbind', distDataByMethod)

    # close iteration through distance metrics
    return(distDataByMethod) })

  # org
  distDataParam = do.call('rbind', distDataParam)

  # close iteration through params
  return(distDataParam) })

#
distData = do.call('rbind', distData)

# org by dyad pair
distData$dyad = with(distData, paste0(Var1, '-', Var2))

# subset to user chosen pairs
distData = distData[distData$dyad %in% dyad, ]

head(distData)
