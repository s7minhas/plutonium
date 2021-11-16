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
# fn to pull out ypm from lat var mods
getConfigDF = function(
  modList, getGOF=FALSE
){

  # iterate through modList and puill out YPM
  configData = lapply(1:length(modList), function(iiConfig){

    # get ii config
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
  write.csv(mats, file=paste0(pathOut, 'latVarGazingCorrel',ii,'.csv'))
   })
####

####
# apply fns to view gof stats
unGOF = getConfigDF(unMods, TRUE)
tradeGOF = getConfigDF(tradeMods, TRUE)
icewsGOF = getConfigDF(icewsMods, TRUE)

#
gofViz = function(gofData){
  ggData = pivot_longer(gofData, cols=sd.rowmean:triad.dep)
  gg=ggplot(
    data = ggData,
    aes(x=config, y=value, color=time) ) +
    geom_point() +
    geom_hline(aes(yintercept=0), color='red', linetype='dashed') +
    facet_wrap(~name, scales='free') +
    theme(
      axis.text.x=element_text(angle=45, hjust=1)
    )
  return(gg) }

# add some logical helpers to separate into blocks for trade
tradeGOF$trade = grepl('trade_k', tradeGOF$config)
tradeGOF$tradeDepSend = grepl('tradeDepSend_k', tradeGOF$config)
tradeGOF$tradeDepSendRaw = grepl('tradeDepSendRaw_k', tradeGOF$config)
tradeGOF$tradeGDP = grepl('tradeGDP_k', tradeGOF$config)
tradeGOF$tradeGDPRaw = grepl('tradeGDPRaw_k', tradeGOF$config)

# add some logical helpers to separate into blocks for icews
icewsGOF$matlCoopGov = grepl('matlCoopGov_k', icewsGOF$config)
icewsGOF$verbCoopGov = grepl('verbCoopGov_k', icewsGOF$config)

#
gofViz(unGOF)
gofViz(tradeGOF[tradeGOF$trade,])
gofViz(tradeGOF[tradeGOF$tradeDepSend,])
gofViz(tradeGOF[tradeGOF$tradeGDP,])
gofViz(icewsGOF[icewsGOF$matlCoopGov,])
gofViz(icewsGOF[icewsGOF$verbCoopGov,])
####
