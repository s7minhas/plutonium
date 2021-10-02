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
	yrs, fname, fpth=paste0(pathOut) ){

	# load object and reassign
	# its name to mod
	yrs <<- yrs ; fname <<- fname
	tmp=ls()
	load(paste0(fpth, fname))
	objName = setdiff(ls(),c(tmp,'tmp'))
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
	names(means)[3] = gsub('.rda','',fname)

	# create id var
	means$id = with(means,
		paste(Var1, Var2, year, sep='_'))

	#
	return(means) }
####

####
# apply process mltr fn
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

#
econScoresTradeDep2 = processLFM(
	yrs=1990:2020,
	fname='econScores_tradeDepSend_lfm_v2.rda')

#
econScoresTrade2 = processLFM(
	yrs=1990:2020,
	fname='econScores_trade_lfm_v2.rda')

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
	econScoresTradeDep2,
	econScoresTrade2,
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
