####
rm(list=ls())
pth = paste0(here::here(), '/')
source(paste0(pth, 'setup.R'))

# load data
load(paste0(pathIn, 'arrList_lfm.rda'))

# lib for lfm
# devtools::install_github('s7minhas/amen')
library(amen)

#
library(doParallel)
library(foreach)
####

####
lfmWrapper = function(
	arrayList,
	yVars,
	iters=5000,
	burn=1000,
	dens=10,
	R=2,
	seed=6886,
	cores=15
){

	#
	cl = makeCluster(cores)
	registerDoParallel(cl)

	#
	ameOut = foreach(
		arr = arrayList,
		.packages=c('amen')) %dopar% {

			# extract relev yvars and
			# convert into format for ame_repL
			arr = arr[,,yVars]
			yL = lapply(1:dim(arr)[3], function(pp){
				return(arr[,,pp]) })

			# run
			out = ame_repL(
					Y = yL,
					rvar=FALSE, cvar=FALSE,
					dcor=FALSE, nvar=FALSE,
					R=2, model='nrm', intercept=FALSE,
					symmetric=FALSE,
					nscan=iters,
					burn=burn, odens=dens, seed=seed,
					plot=FALSE, print=FALSE, gof=FALSE
			)

			# extract relev output
			return( list(U=out$U, V=out$V, UVPM=out$UVPM) )
		}
	stopCluster(cl)

	#
	return(ameOut)
}
####

####
# econ index
econTradeDepScores = lfmWrapper(
	econList,
	c('ptaCnt', 'tradeDepSend') )
save(econTradeDepScores,
	file=paste0(pathOut, 'econScores_tradeDepSend_lfm.rda'))
rm(econTradeDepScores)

econTradeScores = lfmWrapper(
	econList,
	c('ptaCnt', 'trade') )
save(econTradeScores,
	file=paste0(pathOut, 'econScores_trade_lfm.rda'))
rm(econTradeScores)
####

####
# econ index
econTradeDepScores2 = lfmWrapper(
	econList2,
	c('ptaCnt', 'tradeDepSend') )
save(econTradeDepScores2,
	file=paste0(pathOut, 'econScores_tradeDepSend_lfm_v2.rda'))
rm(econTradeDepScores2)

econTradeScores2 = lfmWrapper(
	econList2,
	c('ptaCnt', 'trade') )
save(econTradeScores2,
	file=paste0(pathOut, 'econScores_trade_lfm_v2.rda'))
rm(econTradeScores2)
####

####
# diplom index
diplomScores = lfmWrapper(
	diplomList,
	c('allyTotal', 'agree') )
save(diplomScores,
	file=paste0(pathOut, 'diplomScores_agree_lfm.rda'))
rm(diplomScores)
####

####
# icews index
icewsScores = lfmWrapper(
	icewsList,
	paste0(pasteVec(c('matl', 'verb'), c('Conf', 'Coop')), 'Gov') )
save(icewsScores,
	file=paste0(pathOut, 'icewsScores_gov_lfm.rda'))
rm(icewsScores)
####
