# prep data for net analysis
####
rm(list=ls())
pth = paste0(here::here(), '/')
source(paste0(pth, 'setup.R'))
mltrPth = paste0(pth, 'Funcs/mltrFuncs/')
load(paste0(pathIn, 'frame.rda'))
####

####
# identify id variables
# senders: should be denoted as cname1
# receivers: cname2
# time: year
# dyad-time: id
ids = names(frame)[1:4]
####

####
# fn to prep data for lfm
lfmPrep = function(
	yrs, vars, dyadYearData=frame, idVars=ids
){
	# subset to relevant pd and ftrs
	samp = dyadYearData[
		dyadYearData$year %in% yrs,c(idVars, vars)]

	# convert data to long format
	sampLong = pivot_longer(
		samp, cols=all_of(vars),  names_to='var', values_to='val')

	# cast into arrays separately for each year
	arrList = lapply(yrs, function(tt){
		sampLong = sampLong[sampLong$year==tt,]
		arr = acast(
			sampLong,
			cname1 ~ cname2 ~ var, value.var='val')
		return(arr) })
	names(arrList) = yrs

	#
	return(arrList) }

# fn to prep data in a time series format for lfm
lfmTimePrep = function(
	var, lags, dyadYearData=frame, yrs=1990:2020, idVars=ids
){
	# subset to relevant pd and ftrs
	samp = dyadYearData[
		dyadYearData$year %in% yrs,c(idVars, var)]

	# create grouped time version
	yrBrks = lapply(yrs, function(yr){
		yrCut = (yr-(lags-1)):yr
		return( yrCut[yrCut %in% yrs] ) })

	# divide up samp into buckets
	sampList = lapply(yrBrks, function(yrBrk){

		# only keep obs in relev years
		slice = samp[samp$year %in% yrBrk,]

		# define actors for relev year
		actors = unique( c(
				slice$cname1[slice$year %in% max(yrBrk)],
				slice$cname1[slice$year %in% max(yrBrk)]
			) )

		# subset slice by relev actors
		slice = slice[
			slice$cname1 %in% actors & slice$cname2 %in% actors,]

		#
		return(slice) })

	# cast into arrays separately for each year
	arrList = lapply(sampList, function(slice){
		arr = acast(
			slice,
			cname1~cname2~year, value.var=var )
			return(arr) })
	names(arrList) = yrs

	#
	return(arrList) }
####

####
# econ index
# 1990-2020 (1990 start because of limitations in imf trade data)
# run yearly in case of ame so can start at 1990
econYrs = 1990:2020
econVars = c('ptaCnt', 'trade', 'tradeDepSend')

# apply lfmPrep fn
econList = lfmPrep(econYrs, econVars)
####

####
# econ index - trade only
# 1990-2020 (1990 start because of limitations in imf trade data)
# run yearly in case of ame so can start at 1990
tradeYrs = 1990:2020
tradeVars = c('trade', 'tradeRaw', 'tradeDepSend', 'tradeDepSendRaw')

# apply lfmPrep fn
tradeList = lfmPrep(tradeYrs, tradeVars)
####

####
# econ index - trade only
# layers here will be time, so we will use lagged versions of
# our trade operationalizations with current versions of trade

# apply lfmTimePrep fn to standardized trade
tradeTimeL3List = lfmTimePrep('trade', 3)
tradeTimeL5List = lfmTimePrep('trade', 5)

# apply lfmTimePrep fn to raw trade
tradeRawTimeL3List = lfmTimePrep('tradeRaw', 3)
tradeRawTimeL5List = lfmTimePrep('tradeRaw', 5)

# apply lfmTimePrep fn to standardized trade dep
tradeDepTimeL3List = lfmTimePrep('tradeDepSend', 3)
tradeDepTimeL5List = lfmTimePrep('tradeDepSend', 5)

# apply lfmTimePrep fn to raw trade
tradeDepRawTimeL3List = lfmTimePrep('tradeDepSendRaw', 3)
tradeDepRawTimeL5List = lfmTimePrep('tradeDepSendRaw', 5)
####

####
# diplom array
# 1980-2019 (idpt ends at 2019; atop ends at 2018 but 2019 values were copied over)
diplomYrs = 1980:2019
diplomVars = c('allyTotal', 'agree')

diplomList = lfmPrep(diplomYrs, diplomVars)
####

####
# icews array
# 1995-2020 (icews starts at 1995)
icewsYrs = 1995:2020
icewsVars = pasteVec(
	c('matl','verb'), c('Conf', 'Coop')) %>%
	pasteVec(., c('', 'Gov') )

icewsList = lfmPrep(icewsYrs, icewsVars)
####

###
# diplom + econ
super1List = lfmPrep(econYrs, c(econVars, diplomVars))
###

####
# diplom + econ + icews
super2List = lfmPrep(icewsYrs, c(econVars, diplomVars, icewsVars))
####

####
#
save(
  econList, tradeList,
	tradeTimeL3List, tradeTimeL5List,
	tradeRawTimeL3List, tradeRawTimeL5List,
	tradeDepTimeL3List, tradeDepTimeL5List,
	tradeDepRawTimeL3List, tradeDepRawTimeL5List,
	diplomList, icewsList, super1List, super2List,
  file=paste0(pathIn, 'arrList_lfm.rda')
)
####
