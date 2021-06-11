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
mltrLikeWrapper = function(
	arrayList,
	yVars, xVars,
	cores=10
){
	# run mltr freq in parallel across time periods in array
	cl = makeCluster(cores)
	registerDoParallel(cl)
	mltrOut = foreach(
		arr=arrayList,
		.packages=c('abind'), .export=helperFuncs
	) %dopar% {

		# run
		B = mlm.ALS(
			Y=arr$Y[,,yVars,],
			X=arr$X[,,xVars,],
			verbose=FALSE)

		# cleanup
		A = B[[1]]

		#
		return(A) }

	#
	stopCluster(cl)

	#
	return(mltrOut) }
####

####
# econ index
econScoresML = mltrLikeWrapper(
	econList,
	c('ptaCnt', 'tradeDepSend'),
	c('ptaCnt_ij', 'tradeDepSend_ij') )
save(econScoresML,
	file=paste0(pth, 'results/econScores_tradeDepSend_ML.rda'))
rm(econScoresML)
####

####
# diplom index
diplomScoresML = mltrLikeWrapper(
	diplomList,
	c('allyTotal', 'agree'),
	c('allyTotal_ij', 'agree_ij') )
save(diplomScoresML,
	file=paste0(pth, 'results/diplomScores_agree_ML.rda'))
rm(diplomScoresML)
####

####
# icews index
icewsScoresML = mltrLikeWrapper(
	icewsList,
	paste0(pasteVec(c('matl', 'verb'), c('Conf', 'Coop')), 'Gov'),
	paste0(paste0(pasteVec(c('matl', 'verb'), c('Conf', 'Coop')), 'Gov'), '_ij') )
save(icewsScoresML,
	file=paste0(pth, 'results/icewsScores_gov_ML.rda'))
rm(icewsScoresML)
####
