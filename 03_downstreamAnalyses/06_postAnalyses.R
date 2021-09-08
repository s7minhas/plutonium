####
rm(list=ls())
pth = paste0(here::here(), '/')
source(paste0(pth, 'setup.R'))
dpth = paste0(pathDrop, 'data/')
rpth = paste0(pathDrop, 'results/')

#
pkgs = c(
  'lme4', 'rstanarm', 'ggplot2' )
loadPkg(pkgs)
####

####
# load data and results
# modData, m1, m2, m3, m1b, m2b, m3b
load(paste0(rpth, 'dstreamModels.rda'))
####

####
summary(m1)
summary(m1b)
plot(m1b)
loadPkg('bayesplot')
plot(m1b, 'trace', pars=c('(Intercept)', 'USf1.l1', 'polity.l1', 'GDP.l1', 'pop.l1', 'beijDist', 'washDist', 'IdealPointDistance'))
####
