####
rm(list = ls())
source('setup.R')

#
pkgs = c( 'ggplot2', 'patchwork', 'extrafont', 'Cairo' )
loadPkg(pkgs)

#
source(paste0(rfuncs, 'modSummHelpers.R'))
####

####
load(paste0(rpth, 'modelInfoFin_russia.rda'))
load(paste0(rpth, 'lmerModsFin_russia.rda'))
####

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
  'Capital Distance\nto Russia',
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
  file=paste0(gpth, 'agreeFixedDistract_russia.pdf'),
  device=cairo_pdf)
ggsave(agreeFixedDistract, width=8, height=4,
  file=paste0(gpth, 'agreeFixedDistract_russia.png'),
  dpi=600)
ggsave(agreeVarDistract, width=8, height=8,
  file=paste0(gpth, 'agreeVarDistract_russia.pdf'),
  device=cairo_pdf)
ggsave(agreeVarDistract, width=8, height=8,
  file=paste0(gpth, 'agreeVarDistract_russia.png'),
  dpi=600)
####
