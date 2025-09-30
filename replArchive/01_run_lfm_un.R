####################################
# run latent factor model (lfm) estimation
####################################

# load setup
source('setup.R')

# load pkgs
packs = c('amen', 'doParallel', 'foreach')
loadPkg(packs)

# load prepared array data
load(paste0(dpth, 'arrList_lfm.rda'))

####################################
# define lfm wrapper func
####################################

lfmWrapper = function(
	arrayList,
	yVars=NULL,
	allVars=FALSE,
	iters=5000,
	burn=1000,
	dens=10,
	netDims=2,
	family='nrm',
	seed=6886,
	cores=15,
	srmToggle=FALSE
){
	# par setup
	cl = makeCluster(cores)
	registerDoParallel(cl)

	# iterate through each list element in par
	ameOut = foreach(
		arr = arrayList,
		.packages=c('amen')) %dopar% {

			# extract relev yvars
			if(allVars){ yVars = dimnames(arr)[[3]] }
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
						rvar=srmToggle, cvar=srmToggle,
						dcor=srmToggle, nvar=srmToggle,
						R=netDims, model=family, intercept=srmToggle,
						symmetric=FALSE,
						nscan=iters,
						burn=burn, odens=dens, seed=seed,
						plot=FALSE, print=FALSE, gof=FALSE ) }

			# if single layer then proceed with regular ame
			if(length(dim(arr))==2){

				# run
				out = ame(
						Y = arr,
						rvar=srmToggle, cvar=srmToggle,
						dcor=srmToggle, nvar=srmToggle,
						R=netDims, model=family, intercept=srmToggle,
						symmetric=FALSE,
						nscan=iters,
						burn=burn, odens=dens, seed=seed,
						plot=FALSE, print=FALSE, gof=FALSE ) }

			# extract relev output
			return( list(
				U=out$U, V=out$V, UVPM=out$UVPM,
				GOF=out$GOF, YPM=out$YPM, EZ=out$EZ
			) ) }

	# close cores and return
	stopCluster(cl)
	return(ameOut) }

####################################
# run lfm estimation
####################################

####
# un variants with srm toggles

if(!file.exists(paste0(rpth, 'unMods.rda'))){
	# configs to run
	configs = expand.grid(
		vars = c( 'agree' ),
		netDims = paste0('k',c(2, 5, 8)),
		# time = paste0('t',c(0, 3, 5)),
		nodalEffs = c(T, F) )
	configs$id = apply(configs, 1, paste, collapse='_')
	configs$id = gsub('TRUE','srm_lfm',configs$id)
	configs$id = gsub('FALSE','lfm',configs$id)
	configs$vars = char(configs$vars)
	configs$netDims = char(configs$netDims)
	# configs$time = char(configs$time)

	# iterate through config rows
	unMods = lapply(1:nrow(configs), function(ii){

		# org configs
		var = configs$vars[ii]
		kDim = num(gsub('k','',configs$netDims[ii]))
		# tDim = num(gsub('t','',configs$time[ii]))
		inclSRM = configs$nodalEffs[ii]

		# run mod
		out = lfmWrapper(
			arrayList = diplomList,
			yVars=var, allVars = FALSE,
			netDims = kDim, srmToggle = inclSRM )
		return(out) })

	#
	names(unMods) = configs$id
	unMods = lapply(unMods, function(x){ names(x) = 1980:2019 ; return(x) })
	save(unMods, file=paste0(rpth, 'unMods.rda'))
}
####

####################################