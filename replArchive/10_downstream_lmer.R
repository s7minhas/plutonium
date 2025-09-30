####
rm(list=ls())
source('setup.R')

#
loadPkg(c('lme4', 'doParallel', 'foreach'))
####

####
load(paste0(dpth, "modData.rda"))
load(paste0(rpth, 'modelInfoFin.rda'))
####

####
# exclude US
modData = modData[modData$cname1 != 'UNITED STATES',]
####

####
# rescale indep vars
modData[,ivs] = apply(modData[,ivs], 2, scale)
####

####
vars = c(
  'agree_k2_srm_lfm',
  'lag1_USf1',
  'lag1_USf2',
  'lag1_polity',
  'lag1_gdp',
  'capdist' )
slice = modData[,vars]
names(slice) = c(
  'Diplomatic Alignment', 
  'F1 (Active US Conflicts)', 'F2 (US Defense Spending)',
  'Polity', 'GDP', 'Logged Capital Distance to China')

library(modelsummary)
datasummary_skim(data = slice)
####

ii=1 # dip, f1, int=cname, varInt
ii=3 # dip, f2, int=cname, varInt
ii=7 # dip, f1, int=?, varSlope by polCat

####
# run mods

if(!file.exists(paste0(rpth, 'lmerModsFin.rda'))){
	cores = nrow(modsToRun)
	cl = makeCluster(cores)
	registerDoParallel(cl)
	lmerMods = foreach(ii = 1:nrow(modsToRun), .packages=c('lme4')) %dopar% {

	  # pull out relev info for constructing model form
	  depvar = modsToRun$dv[ii]
	  ivs = ivList[[ modsToRun$ivs[ii] ]]
	  randeff = modsToRun$re[ii]
	  modType = modsToRun$type[ii]

	  # construct formula
	  form = paste0(depvar, '~', ivs, '+ factor(cname1) - 1')
	  if(modType=='varInt'){
	    form = paste0(form, '+ (1|', randeff, ')') }
	  if(modType=='varSlopef1'){
	    form = paste0(form, '+ (lag1_USf1 |', randeff, ')') }
	  if(modType=='varSlopef2'){
	    form = paste0(form, '+ (lag1_USf2 |', randeff, ')') }
	  if(modType=='varSlopef3'){
	    form = paste0(form, '+ (lag1_USf3 |', randeff, ')') }
	  form = formula(form)

	  # run model
	  mod = lmer(form, data=modData)

	  # org fe and re results
	  feSumm = as.data.frame(summary(mod)$'coefficients')
	  feSumm = cbind(
	    grpvar='fixef',
	    term='fixef',
	    iv=rownames(feSumm),
	    feSumm)
	    rownames(feSumm) = NULL
	  reSumm = as.data.frame(ranef(mod))
	  names(reSumm) = c('grpvar','term','iv','Estimate', 'Std. Error')
	  reSumm = cbind(reSumm, 't value'=NA)
	  modSumm = rbind(feSumm, reSumm)

	  # org for saving
	  out = list(
	    formula=form,
	    effects=modSumm,
	    depvar=depvar,
	    ivs=ivs,
	    randeff=randeff,
	    modType=modType )

	  #
	  return(out) }
	stopCluster(cl)

	#
	names(lmerMods) = apply(modsToRun[,-1], 1, paste, collapse='_')
	####

	####
	save(lmerMods, file=paste0(rpth, 'lmerModsFin.rda'))
}
####
