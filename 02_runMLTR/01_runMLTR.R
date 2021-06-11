####
rm(list=ls())
pth = paste0(here::here(), '/')
source(paste0(pth, 'setup.R'))

# load data
dpth = paste0(pth, 'data/')
load(paste0(dpth, 'arrList.rda'))

# fns for mltr
x=ls()
mltrPth = paste0(pth, 'Funcs/mltrFuncs/')
source(paste0(mltrPth, 'mltrHelpers.R'))
source(paste0(mltrPth, "tfunctions.r"))
source(paste0(mltrPth, "functions_als.r"))
source(paste0(mltrPth, "functions_bayes.r"))
source(paste0(mltrPth, "mcmc.r"))
helperFuncs = setdiff(ls(),c(x,'x')) ; rm(x)

#
library(doParallel)
library(foreach)
####

####
mltrWrapper = function(
	arrayList,
	yVars, xVars,
	iters=1500,
	burn=500,
	cores=15
){
	# run mltr in parallel across time periods in array
	cl = makeCluster(cores)
	registerDoParallel(cl)
	mltrOut = foreach(
		arr=arrayList,
		.packages=c('abind'), .export=helperFuncs
	) %dopar% {

		# run
		mod = mltr(
			Y=arr$Y[,,yVars,],
			X=arr$X[,,xVars,],
			NS = iters + burn )

		# cleanup
		A = mod$BPS[[1]]

		# burn and churn
		A = A[,,(burn+1):(burn+iters)]
		return(A) }

	#
	stopCluster(cl)

	#
	return(mltrOut) }
####

####
# econ index
econScores = mltrWrapper(
	econList,
	c('ptaCnt', 'tradeDepSend'),
	c('ptaCnt_ij', 'tradeDepSend_ij') )
save(econScores,
	file=paste0(pth, 'results/econScores_tradeDepSend.rda'))
rm(econScores)
####

####
# diplom index
diplomScores = mltrWrapper(
	diplomList,
	c('allyTotal', 'agree'),
	c('allyTotal_ij', 'agree_ij') )
save(diplomScores,
	file=paste0(pth, 'results/diplomScores_agree.rda'))
rm(diplomScores)
####

####
# icews index
icewsScores = mltrWrapper(
	icewsList,
	paste0(pasteVec(c('matl', 'verb'), c('Conf', 'Coop')), 'Gov'),
	paste0(paste0(pasteVec(c('matl', 'verb'), c('Conf', 'Coop')), 'Gov'), '_ij') )
save(icewsScores,
	file=paste0(pth, 'results/icewsScores_gov.rda'))
rm(icewsScores)
####
