####
pth = paste0(here::here(), '/')
source(paste0(pth, 'setup.R'))
dpth = paste0(pathDrop, 'data/')
rpth = paste0(pathDrop, 'results/')

#
pkgs = c(
  'lme4', 'rstanarm', 'ggplot2', 'maps', 'patchwork' )
loadPkg(pkgs)
####

####
# load data and results
# modData, m1, m2, m3, m1b, m2b, m3b
load(paste0(rpth, 'dstreamModels_cname.rda'))
# load(paste0(rpth, 'dstreamModels_cname.rda'))
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
plot(bf1, 'trace', pars=ivs[-3])
plot(bf2, 'trace', pars=ivs[-2])
plot(bf1HetEff, 'trace', pars=ivs[-(2:3)])
plot(bf2HetEff, 'trace', pars=ivs[-(2:3)])
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
getCoefB(bMats$'bf1', ivs[-3])

getCoefB(bMats$'bf2', ivs[-2])

getCoefB(bMats$'bf1HetEff', ivs[-(2:3)])

getCoefB(bMats$'bf2HetEff', ivs[-(2:3)])
####
