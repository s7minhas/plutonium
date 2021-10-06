####
rm(list=ls())
pth = paste0(here::here(), '/')
source(paste0(pth, 'setup.R'))

# load data
load(paste0(pathIn, 'frame.rda'))
frame = frame[,1:4]
####

####
# fn to extract scores from mltr
processLFM = function(
	yrs, fname, obj=NULL, varName=NULL, fpth=pathOut ){

	# load object and reassign
	# its name to mod
	yrs <<- yrs ; fname <<- fname
	tmp=ls()
	load(paste0(fpth, fname))
	objName = setdiff(ls(),c(tmp,'tmp'))
	if(!is.null(obj)){ objName = obj}
	assign("mod", get(objName))
	rm(list=c(objName))

	# iterate through years and extract mean
	means = lapply(1:length(mod), function(ii){

		# extract posterior means
		muMat = mod[[ii]]$UVPM

		# organize into df
		muDF = reshape2::melt(muMat)
		names(muDF) = c('Var1', 'Var2', 'value')
		muDF = muDF[muDF$Var1 != muDF$Var2,]
		muDF$year = yrs[ii]
		return(muDF) })

	# combine means into a single df
	means = do.call('rbind', means)

	# replace varname
	if(is.null(varName)){
		names(means)[3] = gsub('.rda','',fname)
	} else { names(means)[3] = varName }

	# create id var
	means$id = with(means,
		paste(Var1, Var2, year, sep='_'))

	#
	return(means) }
####

####
# apply process lfm fn
diplomScoresAgree = processLFM(
	yrs=1980:2019,
	fname='diplomScores_agree_lfm.rda')

#
econScoresTradeDep = processLFM(
	yrs=1990:2020,
	fname='econScores_tradeDepSend_lfm.rda')

#
econScoresTrade = processLFM(
	yrs=1990:2020,
	fname='econScores_trade_lfm.rda')

# single layer treaty lfms
treaty_R2 = processLFM(
		yrs=1990:2020, fname='treaty_lfms.rda',
		obj='treaty_R2', varName='treaty_R2_lfm')
treaty_R8 = processLFM(
		yrs=1990:2020, fname='treaty_lfms.rda',
		obj='treaty_R8', varName='treaty_R8_lfm')

# time layer treaty lfms
treatyL3_R2 = processLFM(
		yrs=1990:2020, fname='treaty_lfms.rda',
		obj='treatyL3_R2', varName='treaty_L3_R2_lfm')
treatyL3_R8 = processLFM(
		yrs=1990:2020, fname='treaty_lfms.rda',
		obj='treatyL3_R8', varName='treaty_L3_R8_lfm')
treatyL5_R2 = processLFM(
		yrs=1990:2020, fname='treaty_lfms.rda',
		obj='treatyL5_R2', varName='treaty_L5_R2_lfm')
treatyL5_R8 = processLFM(
		yrs=1990:2020, fname='treaty_lfms.rda',
		obj='treatyL5_R8', varName='treaty_L5_R8_lfm')

# single layer trade lfms
trade_R2 = processLFM(
	yrs=1990:2020, fname='trade_singleLayer_lfms.rda',
	obj='trade_R2', varName='trade_R2_lfm')
tradeDep_R2 = processLFM(
	yrs=1990:2020, fname='trade_singleLayer_lfms.rda',
	obj='tradeDep_R2', varName='tradeDep_R2_lfm')
tradeRaw_R2 = processLFM(
	yrs=1990:2020, fname='trade_singleLayer_lfms.rda',
	obj='tradeRaw_R2', varName='tradeRaw_R2_lfm')
tradeDepRaw_R2 = processLFM(
	yrs=1990:2020, fname='trade_singleLayer_lfms.rda',
	obj='tradeDepRaw_R2', varName='tradeDepRaw_R2_lfm')
trade_R8 = processLFM(
	yrs=1990:2020, fname='trade_singleLayer_lfms.rda',
	obj='trade_R8', varName='trade_R8_lfm')
tradeDep_R8 = processLFM(
	yrs=1990:2020, fname='trade_singleLayer_lfms.rda',
	obj='tradeDep_R8', varName='tradeDep_R8_lfm')
tradeRaw_R8 = processLFM(
	yrs=1990:2020, fname='trade_singleLayer_lfms.rda',
	obj='tradeRaw_R8', varName='tradeRaw_R8_lfm')
tradeDepRaw_R8 = processLFM(
	yrs=1990:2020, fname='trade_singleLayer_lfms.rda',
	obj='tradeDepRaw_R8', varName='tradeDepRaw_R8_lfm')

# time layer trade lfms
tradeL3_R2 = processLFM(
	yrs=1990:2020, fname='trade_timeLayer_lfms.rda',
	obj='tradeL3_R2', varName='trade_L3_R2_lfm')
tradeL5_R2 = processLFM(
	yrs=1990:2020, fname='trade_timeLayer_lfms.rda',
	obj='tradeL5_R2', varName='trade_L5_R2_lfm')
tradeDepL3_R2 = processLFM(
	yrs=1990:2020, fname='trade_timeLayer_lfms.rda',
	obj='tradeDepL3_R2', varName='tradeDep_L3_R2_lfm')
tradeDepL5_R2 = processLFM(
	yrs=1990:2020, fname='trade_timeLayer_lfms.rda',
	obj='tradeDepL5_R2', varName='tradeDep_L5_R2_lfm')
tradeRawL3_R2 = processLFM(
	yrs=1990:2020, fname='trade_timeLayer_lfms.rda',
	obj='tradeRawL3_R2', varName='tradeRaw_L3_R2_lfm')
tradeRawL5_R2 = processLFM(
	yrs=1990:2020, fname='trade_timeLayer_lfms.rda',
	obj='tradeRawL5_R2', varName='tradeRaw_L5_R2_lfm')
tradeDepRawL3_R2 = processLFM(
	yrs=1990:2020, fname='trade_timeLayer_lfms.rda',
	obj='tradeDepRawL3_R2', varName='tradeDepRaw_L3_R2_lfm')
tradeDepRawL5_R2 = processLFM(
	yrs=1990:2020, fname='trade_timeLayer_lfms.rda',
	obj='tradeDepRawL5_R2', varName='tradeDepRaw_L5_R2_lfm')
tradeL3_R8 = processLFM(
	yrs=1990:2020, fname='trade_timeLayer_lfms.rda',
	obj='tradeL3_R8', varName='trade_L3_R8_lfm')
tradeL5_R8 = processLFM(
	yrs=1990:2020, fname='trade_timeLayer_lfms.rda',
	obj='tradeL5_R8', varName='trade_L5_R8_lfm')
tradeDepL3_R8 = processLFM(
	yrs=1990:2020, fname='trade_timeLayer_lfms.rda',
	obj='tradeDepL3_R8', varName='tradeDep_L3_R8_lfm')
tradeDepL5_R8 = processLFM(
	yrs=1990:2020, fname='trade_timeLayer_lfms.rda',
	obj='tradeDepL5_R8', varName='tradeDep_L5_R8_lfm')
tradeRawL3_R8 = processLFM(
	yrs=1990:2020, fname='trade_timeLayer_lfms.rda',
	obj='tradeRawL3_R8', varName='tradeRaw_L3_R8_lfm')
tradeRawL5_R8 = processLFM(
	yrs=1990:2020, fname='trade_timeLayer_lfms.rda',
	obj='tradeRawL5_R8', varName='tradeRaw_L5_R8_lfm')
tradeDepRawL3_R8 = processLFM(
	yrs=1990:2020, fname='trade_timeLayer_lfms.rda',
	obj='tradeDepRawL3_R8', varName='tradeDepRaw_L3_R8_lfm')
tradeDepRawL5_R8 = processLFM(
	yrs=1990:2020, fname='trade_timeLayer_lfms.rda',
	obj='tradeDepRawL5_R8', varName='tradeDepRaw_L5_R8_lfm')

#
icewsScoresGov = processLFM(
	yrs=1995:2020,
	fname='icewsScores_gov_lfm.rda')
####

####
# organize
toMerge = list(
	diplomScoresAgree,
	econScoresTradeDep,
	econScoresTrade,
	treaty_R2, treaty_R8,
	treatyL3_R2, treatyL3_R8,
	treatyL5_R2, treatyL5_R8,
	trade_R2, tradeDep_R2, tradeRaw_R2, tradeDepRaw_R2,
	trade_R8, tradeDep_R8, tradeRaw_R8, tradeDepRaw_R8,
	tradeL3_R2, tradeL5_R2, tradeDepL3_R2, tradeDepL5_R2,
	tradeRawL3_R2, tradeRawL5_R2, tradeDepRawL3_R2, tradeDepRawL5_R2,
	tradeL3_R8, tradeL5_R8, tradeDepL3_R8, tradeDepL5_R8,
	tradeRawL3_R8, tradeRawL5_R8, tradeDepRawL3_R8, tradeDepRawL5_R8,
	icewsScoresGov)

# merge
for(df in toMerge){
	var=names(df)[3]
	frame$tmp = df[match(frame$id, df$id),var]
	names(frame)[ncol(frame)] = var }
####

####
#
lfmScores = frame
save(lfmScores,
	file=paste0(pathOut, 'lfmScores.rda') )
####
