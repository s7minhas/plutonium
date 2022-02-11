####
rm(list=ls())
pth = paste0(here::here(), '/')

#
source(paste0(pth, 'setup.R'))

#
loadPkg(c('lme4', 'doParallel', 'foreach'))
####

####
load(paste0(pathIn, "modData.rda"))
load(paste0(pathOut, 'modelInfo.rda'))
####

####
# rescale indep vars
vars = c(lagDVs, ivs)
modData[,vars] = apply(modData[,vars], 2, scale)
####

####
cntrls = c('lag1_polity', 'lag1_gdp', 'capdist')
kivs = paste0('lag1_USf',1:3)
kivs = kivs[length(kivs)]
ivs = c(kivs, cntrls)
eff = 'cname1'
dvs = c(
  'agree_k2_srm_lfm',
  'tradeDepSend_k2_srm_lfm')

uniEffForms = lapply(dvs, function(dv){
  form = paste0(
    dv, '~' ,
    paste(ivs, collapse='+'),
    '+ (1|', eff,')'
  )
  formula(form)
  })

uniEffMods = lapply(uniEffForms, function(form){
  lmer(form, data=modData)
  })

lapply(uniEffMods, summary)

####

####
# run mods
cores = 20
cl = makeCluster(cores)
registerDoParallel(cl)
lmerMods = foreach(ii = 1:nrow(modsToRun), .packages=c('lme4')) %dopar% {

  # pull out relev info for constructing modfel form
  depvar = modsToRun$dv[ii]
  ivs = ivList[[ modsToRun$ivs[ii] ]]
  randeff = modsToRun$re[ii]
  modType = modsToRun$type[ii]
  inclLag = modsToRun$lagDV[ii]

  # construct formula
  form = paste0(depvar, '~', ivs)
  if(inclLag){ form=paste0(form, '+', paste0('lag1_',depvar)) }
  if(modType=='varInt'){
    form = paste0(form, '+ (1|', randeff, ')') }
  if(modType=='varSlope_f1'){
    form = paste0(form, '+ (1 + lag1_USf1 |', randeff, ')') }
  if(modType=='varSlope_f2'){
    form = paste0(form, '+ (1 + lag1_USf2 |', randeff, ')') }
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
    modType=modType,
    inclLag=inclLag )

  #
  return(out) }
stopCluster(cl)
####

####
save(lmerMods, file=paste0(pathOut, 'lmerMods.rda'))
####
