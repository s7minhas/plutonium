# prep data for net analysis
####
rm(list=ls())
pth = paste0(here::here(), '/')
source(paste0(pth, 'setup.R'))
mltrPth = paste0(pth, 'Funcs/mltrFuncs/')
load(paste0(pathIn, 'frame.rda'))
source(paste0(pth, 'setup.R'))
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
			cname1 ~ cname2 ~ var, value.var='val'
		)
		return(arr)
	})

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
diplom + econ
super1List = lfmPrep(econYrs, c(econVars, diplomVars))
###

####
# diplom + econ + icews
super2List = lfmPrep(icewsYrs, c(econVars, diplomVars, icewsVars))
####

####
#
save(
  econList, diplomList, icewsList, super1List, super2List,
  file=paste0(pathIn, 'arrList_lfm.rda')
)
####
