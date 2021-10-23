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
gofViz(tradeGOF[tradeGOF$tradeDepSendRaw,])
gofViz(tradeGOF[tradeGOF$tradeGDP,])
gofViz(icewsGOF[icewsGOF$matlCoopGov,])
gofViz(icewsGOF[icewsGOF$verbCoopGov,])
####

####
# visualize mult eff space

####


# # cntry key based on data
#
# cntryKey = rownames(yhat) %>%
# 	data.frame(
# 		cname=.,
# 		cown=countrycode(.,'country.name','cown'),
# 		stringsAsFactors = FALSE)
# cntryKey$cown[
# 	cntryKey$cname=="KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF"]='731'
# cntryKey$cown[
# 	cntryKey$cname=="SERBIA"]='345'
# cntryKey$cowc = countrycode(
# 	cntryKey$cname, 'country.name', 'cowc')
# cntryKey$cowc[cntryKey$cname=='SERBIA'] = 'YUG'
#
# # relabel U, yhat, and V
# rownames(yhat) = colnames(yhat) = cntryKey$cowc
# rownames(U) = rownames(V) = cntryKey$cowc
#
# # geo colors for nodes
# # loadPkg('cshapes')
# cmap = wmap = cshp(date=as.Date('2016-1-1'))
# wmap$cowc = countrycode(wmap$COWCODE, 'cown', 'cowc')
# wmap$cowc[wmap$COWCODE==731]='PRK'
# wmap = wmap[which(as.character(wmap$cowc) %in% cntryKey$cowc),]
# coords=coordinates(wmap) ; rownames(coords)=wmap$cowc
# coords=coords[cntryKey$cowc,]
#
# # Create colors
# rlon = pi*coords[,1]/180 ; rlat = pi*coords[,2]/180
# slon =  (rlon-min(rlon))/(max(rlon)-min(rlon))
# slat =  (rlat-min(rlat))/(max(rlat)-min(rlat))
# ccols = rgb( slon^2,slat^2,(1-sqrt(slon*slat))^2)
# names(ccols) = cntryKey$cowc ; cntryKey$ccols = ccols
#
# # Generate legend map
# cmap@data$cowc = countrycode(cmap@data$COWCODE, 'cown', 'cowc')
# cmap@data$cowc[cmap@data$COWCODE==731]='PRK'
# mapCol = ccols[match(cmap$cowc, cntryKey$cowc)]
# mapCol[is.na(mapCol)] = 'grey' ; names(mapCol) = cmap@data$cowc
# cmapDF=fortify(cmap,region='FEATUREID') ; names(cmapDF)[6]='FEATUREID' ; cmapDF=join(cmapDF, cmap@data)
# ggMap = ggplot() +
# 	geom_polygon(data=cmapDF, aes(x=long, y=lat,group=group,fill=cowc),color='grey30',size=.05) +
# 	scale_fill_manual(values=mapCol) +
# 	coord_equal() + xlab('') + ylab('') +
# 	theme_bw() +
# 	theme(
# 		legend.position = 'none',
# 		panel.border = element_blank(), panel.grid=element_blank(),
# 		axis.ticks = element_blank(), axis.line=element_blank(),
# 		axis.text = element_blank() )
# ggsave(ggMap, file=paste0(pathGraphics, 'mapLeg.png'))

modList = unMods ; iiConfig = 3 ; tt = 1
    # get ii config
    modConfig = modList[[iiConfig]]
      # get tt mod from selected config
      modSlice = modConfig[[tt]]
yhat = modSlice$'YPM'
U = modSlice$'U'
V = modSlice$'V'


# load back in so we can add to circ
loadPkg(c('grid', 'png'))
mapForCirc = rasterGrob(readPNG(paste0(pathGraphics, 'mapLeg.png')), interpolate=TRUE)
load(paste0(pathGraphics, 'mapCol.rda'))

# relabel id attrs in mats
ids=cntryKey$cowc[match(rownames(yhat), cntryKey$cname)]
rownames(yhat) = colnames(yhat) = rownames(U) = rownames(V) = ids


#
source(paste0(pth, 'funcs/ameHelpers.R'))
# plot
toLabel=c("IRN","IRQ","SYR","PRK",'LIB','CHN','RUS','USA','GMY','CAN','UKG','ISR')
other=names(sort(rowSums(yhat, na.rm=TRUE) + colSums(yhat, na.rm=TRUE), decreasing=TRUE))[1:50]
tots = c(toLabel,other)

ggU = getDataForCirc(Y=yhat, U=U, V=NULL, vscale=.65,removeIsolates=FALSE)$uG
ggU = unique(ggU)
ggU$ccols = cntryKey$ccols[match(ggU$actor,cntryKey$cowc)]
ggU$lab = ggU$actor
ggU$lab[!ggU$lab %in% toLabel] = ''
ggU$lPch = ggU$tPch ; ggU$lPch[ggU$lab==''] = 0

circViz = ggplot(ggU, aes(x=X1, y=X2, size=tPch, color=actor)) +
	annotation_custom(mapForCirc, xmin=-.75, xmax=.75, ymin=-.75, ymax=.75) +
	geom_point(alpha=.9) + scale_size(range=c(4,8)) +
	ylab("") + xlab("") +
	geom_label_repel(aes(label=lab, size=lPch)) +
	scale_color_manual(values=ccols) +
	theme_bw() +
	theme(
		legend.position = 'none',
		panel.border = element_blank(), panel.grid=element_blank(),
		axis.ticks = element_blank(), axis.line=element_blank(),
		axis.text = element_blank()
		)

circViz
