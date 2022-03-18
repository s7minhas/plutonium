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

modData$agree_k2_srm_lfm = modData$agree_k5_srm_lfm
modData$tradeDepSend_k2_srm_lfm = modData$tradeDepSend_k5_srm_lfm

####
# exclude US
modData = modData[modData$cname1 != 'UNITED STATES',]
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

#
pkgs = c( 'ggplot2', 'patchwork', 'extrafont', 'Cairo' )
loadPkg(pkgs)

#
source(paste0(pathFuncs, 'modSummHelpers.R'))

####
# melt mod info into df
modData = lapply(1:length(lmerMods), function(ii){
  mod = lmerMods[[ii]]
  eff = mod$effects
  eff$model = names(lmerMods)[ii]
  eff$dv = mod$depvar
  eff$re = mod$randeff
  eff$type = mod$varInt
  return(eff)
  return(eff) })
modData = do.call('rbind', modData)

# relabel f1 vars
modData$modLab = modData$model
modData$modLab[grepl('f1',modData$modLab)] = 'F1 (Active US Conflicts)'
modData$modLab[grepl('f2',modData$modLab)] = 'F2 (US Defense Spending)'
modData$modLab[grepl('f3',modData$modLab)] = 'F3 (US Economic Shocks)'
####

####
# create variable key for fixef labels
varKey = data.frame(
  dirty=unique(modData$iv[modData$grpvar=='fixef']),
  stringsAsFactors=FALSE )
varKey$clean = c(
  '(Intercept)',
  'United States\nConstraint Proxy',
  'Polity',
  'GDP',
  'Capital Distance\nto China',
  'United States\nConstraint Proxy',
  'United States\nConstraint Proxy' )
varKey$clean = factor(
  varKey$clean,
  levels=rev(varKey$clean[c(2,3:5,1)]) )

# add cleaned variable to df
modData$ivClean = varKey$clean[match(modData$iv, varKey$dirty)]
####

####
# remove f3 due to lack of clarity
modData = modData[!grepl('f3', modData$model),]
####

####
# viz mods with fixed f1 params
agreeFixedDistract = coefViz(
  coefProcess(
    modData[
      modData$dv=='agree_k2_srm_lfm' &
      modData$re=='cname1' &
      modData$grpvar=='fixef',]  ) )

tradeFixedDistract = coefViz(
  coefProcess(
    modData[
      modData$dv=='tradeDepSend_k2_srm_lfm' &
      modData$re=='cname1' &
      modData$grpvar=='fixef',]  ) )
####

####
# viz mods with varying f1 params
modData$modLab[grepl('f1',modData$model)] = 'Fixed Effects with varying F1'
modData$modLab[grepl('f2',modData$model)] = 'Fixed Effects with varying F2'
# modData$modLab[grepl('f3',modData$model)] = 'Fixed Effects with varying F3'

# fixed eff results
agreeVarDistractFixed = coefViz(
  coefProcess(
    modData[
      modData$dv=='agree_k2_srm_lfm' &
      modData$re=='polCat3' &
      modData$grpvar=='fixef',]  ) )

tradeVarDistractFixed = coefViz(
  coefProcess(
    modData[
      modData$dv=='tradeDepSend_k2_srm_lfm' &
      modData$re=='polCat3' &
      modData$grpvar=='fixef',]  ) )

# rand eff results
reData = modData[
  modData$re=='polCat3' &
  modData$grpvar=='polCat3' &
  modData$term!='(Intercept)',]

# relabel f1 vars
reData$modLab = reData$model
reData$modLab[grepl('f1',reData$model)] = 'Varying Slopes of F1\nby Polity Categories'
reData$modLab[grepl('f2',reData$model)] = 'Varying Slopes of F2\nby Polity Categories'
# reData$modLab[grepl('f3',reData$model)] = 'Varying Slopes of F3\nby Polity Categories'

# clean up vars
reData$ivClean = reData$iv
reData$ivClean[reData$iv=='(5, Inf]'] = 'Democracy'
reData$ivClean[reData$iv=='(-Inf,-6]'] = 'Autocracy'
reData$ivClean[reData$iv=='(-6,5]'] = 'Anocracy'
reData$ivClean = factor(
  reData$ivClean,
  levels=c('Democracy', 'Anocracy', 'Autocracy') )

# get varying slope viz
agreeVarDistractRE = coefViz(
  coefProcess(
    reData[reData$dv=='agree_k2_srm_lfm',] ) )

tradeVarDistractRE = coefViz(
  coefProcess(
    reData[reData$dv=='tradeDepSend_k2_srm_lfm',] ) )

# combine
agreeVarDistract = agreeVarDistractRE/agreeVarDistractFixed
tradeVarDistract = tradeVarDistractRE/tradeVarDistractFixed
####

####
# save
ggsave(agreeFixedDistract, width=8, height=4,
  file=paste0(pathPaper, 'agreeFixedDistract_k5.pdf'),
  device=cairo_pdf)
ggsave(tradeFixedDistract, width=8, height=4,
  file=paste0(pathPaper, 'tradeFixedDistract_k5.pdf'),
  device=cairo_pdf)
ggsave(agreeVarDistract, width=8, height=8,
  file=paste0(pathPaper, 'agreeVarDistract_k5.pdf'),
  device=cairo_pdf)
ggsave(tradeVarDistract, width=8, height=8,
  file=paste0(pathPaper, 'tradeVarDistract_k5.pdf'),
  device=cairo_pdf)
####
