###Visualization stuff
rm(list=ls())
pth = paste0(here::here(), '/')
source(paste0(pth, 'setup.R'))

#
loadPkg(c(
  'reshape2', 'amen',
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
  modList, getGOF=FALSE
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

      # get gof obs
      if(getGOF){

        # get gof stats
        gofObs = as.vector(modSlice$'GOF')
        gofPred = gofstats(modSlice$'YPM')
        gofDiff = gofPred - gofObs

        # organize
        gofDF = data.frame(matrix(gofDiff, nrow=1))
        names(gofDF) = colnames(modSlice$'GOF')
        gofDF$config = names(modList)[iiConfig]
        gofDF$time = tt

        #
        return(gofDF) }

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
  if(!getGOF){
    configDF = configData[[1]]
    for(ii in 2:length(configData)){
      toAdd = configData[[ii]][,4]
      configDF = cbind(configDF, toAdd)
      names(configDF)[ncol(configDF)] = names(modList)[ii] } }

  # gof case
  if(getGOF){
    configDF = do.call('rbind', configData) }

  #
  return(configDF) }
####

####
# apply fns to view gof stats
unGOF = getConfigDF(unMods, TRUE)
tradeGOF = getConfigDF(tradeMods, TRUE)
icewsGOF = getConfigDF(icewsMods, TRUE)
####

####
# apply fn to get out YPM from mods and examine correlations
unDF = getConfigDF(unMods)
tradeDF = getConfigDF(tradeMods)
icewsDF = getConfigDF(icewsMods)

# reorg into list
dfs = list(unDF, tradeDF, icewsDF)
lapply(dfs, head)

# write correlations to csv
lapply(1:length(dfs), function(ii){
  df = dfs[[ii]]
  mats = round(cor(df[,-(1:3)]), 2)
  write.csv(mats, file=paste0('C:/Users/Owner/Desktop/tmp',ii,'.csv'))
   })
####
