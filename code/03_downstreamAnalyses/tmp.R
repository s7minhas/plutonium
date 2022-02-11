####
pth = paste0(here::here(), '/')
source(paste0(pth, 'setup.R'))
dpth = paste0(pathDrop, 'data/')
rpth = paste0(pathDrop, 'results/')

#
pkgs = c(
  'lme4', 'rstanarm', 'knitr' )
loadPkg(pkgs)
####

####
load(paste0(rpth, 'modelInfo.rda'))
load(paste0(rpth, 'lmerMods.rda'))
load(paste0(rpth, 'stanMods.rda'))
####

########################
# get vec of final mods

####
# mods with f1 iv and random country intercepts
agree2_f1_cname_int = with(modsToRun, which(
  dv=='agree_k2_srm_lfm' & ivs=='ivf1' &
  lagDV==FALSE & re=='cname1' & type=='varInt' ))

trade2_f1_cname_int = with(modsToRun, which(
  dv=='tradeDepSend_k2_srm_lfm' & ivs=='ivf1' &
  lagDV==FALSE & re=='cname1' & type=='varInt' ))
####

####
# mods with f2 iv and random country intercepts
agree2_f2_cname_int = with(modsToRun, which(
  dv=='agree_k2_srm_lfm' & ivs=='ivf2' &
  lagDV==FALSE & re=='cname1' & type=='varInt' ))

trade2_f2_cname_int = with(modsToRun, which(
  dv=='tradeDepSend_k2_srm_lfm' & ivs=='ivf2' &
  lagDV==FALSE & re=='cname1' & type=='varInt' ))
####

####
# mods with base iv and polCat rand effs with f1
agree2_base_polCat_f1 = with(modsToRun, which(
  dv=='agree_k2_srm_lfm' & ivs=='ivBase' &
  lagDV==FALSE & re=='polCat3' & type=='varSlope_f1' ))

trade2_base_polCat_f1 = with(modsToRun, which(
  dv=='tradeDepSend_k2_srm_lfm' & ivs=='ivBase' &
  lagDV==FALSE & re=='polCat3' & type=='varSlope_f1' ))
####

####
# mods with base iv and polCat rand effs with f2
agree2_base_polCat_f2 = with(modsToRun, which(
  dv=='agree_k2_srm_lfm' & ivs=='ivBase' &
  lagDV==FALSE & re=='polCat3' & type=='varSlope_f2' ))

trade2_base_polCat_f2 = with(modsToRun, which(
  dv=='tradeDepSend_k2_srm_lfm' & ivs=='ivBase' &
  lagDV==FALSE & re=='polCat3' & type=='varSlope_f2' ))
####

toKeep = c(
  agree2_f1_cname_int,
  agree2_f2_cname_int,
  agree2_base_polCat_f1,
  agree2_base_polCat_f2,
  trade2_f1_cname_int,
  trade2_f2_cname_int,
  trade2_base_polCat_f1,
  trade2_base_polCat_f2 )

#
stanMods = stanMods[toKeep]
lmerMods = lmerMods[toKeep]

#
save(lmerMods, stanMods, file=paste0(rpth, 'finMods.rda'))

#
names(stanMods[[ length(stanMods) ]])
names(lmerMods[[ length(stanMods) ]])

#
stanMods[[ length(stanMods) ]]$effects
lmerMods[[ length(stanMods) ]]$effects

lapply(stanMods, function(x){x$effects})
########################


####
# load data and results
# modData, m1, m2, m3, m1b, m2b, m3b
load(paste0(rpth, 'dstreamModels_cname.rda'))
load(paste0(rpth, 'dstreamModels_region.rda'))
####

####
ivs = c(
  '(Intercept)',
  'USf1.l1',
  'USf2.l1',
  'polity.l1',
  'GDP.l1', 'pop.l1',
  'beijDist',
  'IdealPointDistance' )
####

####
# check convergence
# plot(bf1, 'trace', pars=ivs[-3])
# plot(bf2, 'trace', pars=ivs[-2])
# plot(bf1HetEff, 'trace', pars=ivs[-(2:3)])
# plot(bf2HetEff, 'trace', pars=ivs[-(2:3)])
####

####
# pull out mats
bMats = lapply(
  list(bf1, bf2, bf1HetEff, bf2HetEff),
  function(x){ as.matrix(x) } )
names(bMats) = c('bf1', 'bf2', 'bf1HetEff', 'bf2HetEff')
####

####
#  quick comparison of iv effs
kable(getCoefB(bMats$'bf1', ivs[-3]))

kable(getCoefB(bMats$'bf2', ivs[-2]))

kable(getCoefB(bMats$'bf1HetEff', ivs[-(2:3)]))

kable(getCoefB(bMats$'bf2HetEff', ivs[-(2:3)]))
####
