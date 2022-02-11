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
load(paste0(rpth, 'finMods.rda'))
####

lapply(lmerMods, function(x){ head(x$effects) })
