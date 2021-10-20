###Visualization stuff
rm(list=ls())
pth = paste0(here::here(), '/')
source(paste0(pth, 'setup.R'))

#
loadPkg(c(
  'reshape2',
  'ggplot2', 'tidyverse',
  'cshapes', 'countrycode' ))
####

####
load(file=paste0(pathOut, 'tradeMods.rda')) # tradeMods
load(file=paste0(pathOut, 'unMods.rda')) # unMods
load(file=paste0(pathOut, 'icewsMods.rda')) # icewsMods
####

####
# fn to pull out ypm from lat var mods
getConfigDF = function(
  modList
){

  # iterate through modList and puill out YPM
  configData = lapply(1:length(modList), function(iiConfig){

    # get ii config unMOd
    modConfig = modList[[iiConfig]]

    # now iterate through time
    modT = lapply(1:length(modConfig), function(tt){

      # get tt mod from selected config
      modSlice = modConfig[[tt]]

      # pull out the YPM matrix
      yhat = modSlice$'YPM'

      # reorg into column
      yhat = reshape2::melt(yhat)
      yhat = na.omit(yhat)

      # add in ids
      names(yhat)[ncol(yhat)] = names(modList)[iiConfig]
      yhat$time = tt
      yhat = yhat[,c(1:2,4,3)]

      # cleanup
      yhat$Var1 = char(yhat$Var1)
      yhat$Var2 = char(yhat$Var2)

      #
      return(yhat) })

    #
    modT = do.call('rbind', modT)

    #
    return(modT) })

  # merge into one df
  configDF = configData[[1]]
  for(ii in 2:length(configData)){
    toAdd = configData[[ii]][,4]
    configDF = cbind(configDF, toAdd)
    names(configDF)[ncol(configDF)] = names(modList)[ii] }

  #
  return(configDF) }

# apply fn to mods
unDF = getConfigDF(unMods)
tradeDF = getConfigDF(tradeMods)
icewsDF = getConfigDF(icewsMods)
####

####
# correlations

####
