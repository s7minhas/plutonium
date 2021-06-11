# prep data for net analysis
####
rm(list=ls())
pth = paste0(here::here(), '/')
mltrPth = paste0(pth, 'Funcs/mltrFuncs/')
dpth = paste0(pth, 'data/')
load(paste0(dpth, 'frame.rda'))
source(paste0(pth, 'setup.R'))
source(paste0(mltrPth, 'mltrHelpers.R'))
source(paste0(mltrPth, 'tfunctions.R'))
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
# econ index
# 1990-2020 (1990 start because of limitations in imf trade data)
# need approx five years of data to get reasonable estimates from the model
# so start at 1995
econYrs = 1995:2020
econVars = c('ptaCnt', 'trade', 'tradeDepSend')

# get list of arrays, fn here is from mltrHelpers.R
econList = getListArrays(
  timePd = econYrs,
  timeRange = 5,
  idVars = ids,
  valVars = econVars
)
####

####
# diplom array
# 1980-2019 (idpt ends at 2019; atop ends at 2018 but 2019 values were copied over)
diplomYrs = 1980:2019
diplomVars = c('allyTotal', 'agree')

#
diplomList = getListArrays(diplomYrs, 5, ids, diplomVars)
####

####
# icews array
# 1995-2020 (icews starts at 1995)
icewsYrs = 2000:2020
icewsVars = pasteVec(
	c('matl','verb'), c('Conf', 'Coop')) %>%
	pasteVec(., c('', 'Gov') )

icewsList = getListArrays(icewsYrs, 5, ids, icewsVars)
####

####
# diplom + econ
super1List = getListArrays(econYrs, 5, ids, c(econVars, diplomVars))
####

####
# diplom + econ + icews
super2List = getListArrays(icewsYrs, 5, ids, c(econVars, diplomVars, icewsVars))
####

####
#
save(
  econList, diplomList, icewsList, super1List, super2List,
  file=paste0(dpth, 'arrList.rda')
)
####
