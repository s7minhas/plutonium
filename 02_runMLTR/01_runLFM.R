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
	yVars=NULL,
	iters=5000,
	burn=1000,
	dens=10,
	netDims=2,
	seed=6886,
	cores=15
){

	# par setup
	cl = makeCluster(cores)
	registerDoParallel(cl)

	# iterate through each list element in par
	ameOut = foreach(
		arr = arrayList,
		.packages=c('amen')) %dopar% {

			# extract relev yvars
			if(is.null(yVars)){ yVars = dimnames(arr)[[3]] }
			arr = arr[,,yVars]

			# if multi layers convert into format
			# for ame_repL and run
			if(length(dim(arr))>2){

				# convert dv into ame_repL format
				yL = lapply(1:dim(arr)[3], function(pp){
					return(arr[,,pp]) })

				# run
				out = ame_repL(
						Y = yL,
						rvar=FALSE, cvar=FALSE,
						dcor=FALSE, nvar=FALSE,
						R=netDims, model='nrm', intercept=FALSE,
						symmetric=FALSE,
						nscan=iters,
						burn=burn, odens=dens, seed=seed,
						plot=FALSE, print=FALSE, gof=FALSE ) }

			# if single layer then proceed with regular ame
			if(length(dim(arr))==2){

				# run
				out = ame(
						Y = arr,
						rvar=FALSE, cvar=FALSE,
						dcor=FALSE, nvar=FALSE,
						R=netDims, model='nrm', intercept=FALSE,
						symmetric=FALSE,
						nscan=iters,
						burn=burn, odens=dens, seed=seed,
						plot=FALSE, print=FALSE, gof=FALSE ) }

			# extract relev output
			return( list(U=out$U, V=out$V, UVPM=out$UVPM) ) }

	# close cores and return
	stopCluster(cl)
	return(ameOut) }
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
# trade indices
# single layer nets
trade_R2 = lfmWrapper(tradeList, 'trade' )
tradeDep_R2 = lfmWrapper(tradeList, 'tradeDepSend' )
tradeRaw_R2 = lfmWrapper(tradeList, 'tradeRaw' )
tradeDepRaw_R2 = lfmWrapper(tradeList, 'tradeDepSendRaw' )

trade_R8 = lfmWrapper(tradeList, 'trade', netDims=8 )
tradeDep_R8 = lfmWrapper(tradeList, 'tradeDepSend', netDims=8 )
tradeRaw_R8 = lfmWrapper(tradeList, 'tradeRaw', netDims=8 )
tradeDepRaw_R8 = lfmWrapper(tradeList, 'tradeDepSendRaw', netDims=8 )

# save
save(
	trade_R2, tradeDep_R2, tradeRaw_R2, tradeDepRaw_R2,
	trade_R8, tradeDep_R8, tradeRaw_R8, tradeDepRaw_R8,
	file=paste0(pathOut, 'trade_singleLayer_lfms.rda')
)

# cleanup
rm(
	trade_R2, tradeDep_R2, tradeRaw_R2, tradeDepRaw_R2,
	trade_R8, tradeDep_R8, tradeRaw_R8, tradeDepRaw_R8 )
####

####
# trade indices
# time layer nets
tradeL3_R2 = lfmWrapper(tradeTimeL3List, netDims=2)
tradeL5_R2 = lfmWrapper(tradeTimeL5List, netDims=2)
tradeDepL3_R2 = lfmWrapper(tradeDepTimeL3List, netDims=2)
tradeDepL5_R2 = lfmWrapper(tradeDepTimeL5List, netDims=2)
tradeRawL3_R2 = lfmWrapper(tradeRawTimeL3List, netDims=2)
tradeRawL5_R2 = lfmWrapper(tradeRawTimeL5List, netDims=2)
tradeDepRawL3_R2 = lfmWrapper(tradeDepRawTimeL3List, netDims=2)
tradeDepRawL5_R2 = lfmWrapper(tradeDepRawTimeL5List, netDims=2)

tradeL3_R8 = lfmWrapper(tradeTimeL3List, netDims=8)
tradeL5_R8 = lfmWrapper(tradeTimeL5List, netDims=8)
tradeDepL3_R8 = lfmWrapper(tradeDepTimeL3List, netDims=8)
tradeDepL5_R8 = lfmWrapper(tradeDepTimeL5List, netDims=8)
tradeRawL3_R8 = lfmWrapper(tradeRawTimeL3List, netDims=8)
tradeRawL5_R8 = lfmWrapper(tradeRawTimeL5List, netDims=8)
tradeDepRawL3_R8 = lfmWrapper(tradeDepRawTimeL3List, netDims=8)
tradeDepRawL5_R8 = lfmWrapper(tradeDepRawTimeL5List, netDims=8)

# save
save(
	tradeL3_R2, tradeL5_R2, tradeDepL3_R2, tradeDepL5_R2,
	tradeRawL3_R2, tradeRawL5_R2, tradeDepRawL3_R2, tradeDepRawL5_R2,
	tradeL3_R8, tradeL5_R8, tradeDepL3_R8, tradeDepL5_R8,
	tradeRawL3_R8, tradeRawL5_R8, tradeDepRawL3_R8, tradeDepRawL5_R8,
	file=paste0(pathOut, 'trade_timeLayer_lfms.rda')
)

# cleanup
rm(
	tradeL3_R2, tradeL5_R2, tradeDepL3_R2, tradeDepL5_R2,
	tradeRawL3_R2, tradeRawL5_R2, tradeDepRawL3_R2, tradeDepRawL5_R2,
	tradeL3_R8, tradeL5_R8, tradeDepL3_R8, tradeDepL5_R8,
	tradeRawL3_R8, tradeRawL5_R8, tradeDepRawL3_R8, tradeDepRawL5_R8
)
####

getwd()

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
