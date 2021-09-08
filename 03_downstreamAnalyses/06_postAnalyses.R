####
pth = paste0(here::here(), '/')
source(paste0(pth, 'setup.R'))
dpth = paste0(pathDrop, 'data/')
rpth = paste0(pathDrop, 'results/')

#
pkgs = c(
  'lme4', 'rstanarm', 'ggplot2' )
loadPkg(pkgs)

# mod summ fns
digs=3
getCoef = function(x){
  round(cbind(mu=fixef(x), sd=sqrt(diag(vcov(x)))),digs) }

getCoefB = function(x, vars, int = FALSE){
  out = cbind(
    mu=apply(x[,vars], 2, mean) ,
    sd=apply(x[,vars], 2, sd) )
  if(int){
    out = cbind(out,
      qtLo95 = apply(x[,vars], 2, quantile, .025),
      qtHi95 = apply(x[,vars], 2, quantile, .975) ) }
  out = round( out, digs )
  return(out) }
####

####
# load data and results
# modData, m1, m2, m3, m1b, m2b, m3b
load(paste0(rpth, 'dstreamModels.rda'))
####

####
ivs = c(
  '(Intercept)',
  'USf1.l1', 'polity.l1',
  'GDP.l1', 'pop.l1',
  'beijDist', 'washDist',
  'IdealPointDistance' )
####

####
# check convergence
plot(bf1, 'trace', pars=ivs)
plot(bf2, 'trace', pars=ivs[-2])
plot(bf1HetEff, 'trace', pars=ivs[-2])
plot(bf2HetEff, 'trace', pars=ivs[-2])
####

####
# pull out mats
bMats = lapply(
  list(m1b, m2b, m3b, m1bZ, m2bZ, m3bZ),
  function(x){ as.matrix(x) } )
names(bMats) = c(
  paste0('m',1:3,'b'),
  paste0('m',1:3,'bZ') )
####

cbind(unique(modData$region2))

####
#  quick comparison of iv effs
getCoef(m1)
getCoefB(bMats$'m1b', ivs)

plot(m1b, pars=ivs)

getCoef(m2)
getCoefB(bMats$'m2b', ivs[-2])

plot(m2b, pars=ivs[-2])

getCoef(m3)
getCoefB(bMats$'m3b', ivs[-2])

plot(m3b, pars=ivs[-2])
####

####
# ran effects
effs = setdiff(colnames(bMats$'m2b'), ivs)
effs = effs[!grepl('(Intercept)', effs)]
getCoefB(bMats$'m2b', effs, T)

plot(m2b, pars=effs)

effs = setdiff(colnames(bMats$'m3b'), ivs)
effs = effs[!grepl('(Intercept)', effs)]
getCoefB(bMats$'m3b', effs, T)

plot(m3b, pars=effs)
####
