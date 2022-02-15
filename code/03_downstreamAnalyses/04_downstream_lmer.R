####
rm(list=ls())
pth = paste0(here::here(), '/')
source(paste0(pth, 'setup.R'))

#
loadPkg(c('lme4', 'doParallel', 'foreach'))
####

####
load(paste0(pathIn, "modData.rda"))
load(paste0(pathOut, 'modelInfoFin.rda'))
####

####
# rescale indep vars
modData[,ivs] = apply(modData[,ivs], 2, scale)
####

####
# run mods
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
  form = paste0(depvar, '~', ivs)
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
save(lmerMods, file=paste0(pathOut, 'lmerModsFin.rda'))
####
