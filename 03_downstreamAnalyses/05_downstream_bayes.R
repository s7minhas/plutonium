####
rm(list=ls())
pth = paste0(here::here(), '/')
source(paste0(pth, 'setup.R'))
source(paste0(pth, 'lagger.R'))
dpth = paste0(pathDrop, 'data/')
rpth = paste0(pathDrop, 'results/')

#
pkgs = c(
  'mlmRev', 'lme4', 'rstanarm', 'ggplot2', 'countrycode' )
loadPkg(pkgs)
####

####
# load data
load(paste0(dpth, "chiData.rda"))
####

####
# processing

# vars for analysis
useVars = c(
  "USf1.l1", "USf2.l1", "USf3.l1", "polity.l1",
  "GDP.l1", "pop.l1", "beijDist", "washDist", "IdealPointDistance")
use = c(
  useVars,
  "econScores_tradeDepSend", "diplomScores_agree", "icewsScores_gov",
  "econScores_tradeDepSend_lfm", "diplomScores_agree_lfm", "icewsScores_gov_lfm")

# add region dummies
regionMat = model.matrix(~chiData$region)
regionMat = data.frame(regionMat)
chiData = cbind(chiData, regionMat)

# set up region codings
chiData$region2 = countrycode(chiData$cname1, "country.name", "region23")
chiData$chinaRegions = chiData$region2 %in% c("Eastern Asia", "Australia and New Zealand", "South-Eastern Asia")

# log vars
chiData$GDP.l1 = log(chiData$GDP.l1 + 1)
chiData$pop.l1 = log(chiData$pop.l1 + 1)

# stdz
chiData$beijDist = scale(chiData$beijDist)
chiData$washDist = scale(chiData$washDist)

# lag vars and create diff version of dvs
chiData$econScores_tradeDepSend.l1 = lagger(chiData$econScores_tradeDepSend, chiData$cname1, chiData$year, 1)
chiData$econDelta = chiData$econScores_tradeDepSend - chiData$econScores_tradeDepSend.l1
chiData$econDelta_lfm = chiData$econScores_tradeDepSend_lfm - chiData$econScores_tradeDepSend_lfm.l1
####

####
# relabel as modData so we can save with models
modData = chiData
####

####
m1 = lmer(
  econDelta_lfm ~
    USf1.l1 + polity.l1 + GDP.l1 + pop.l1 +
    beijDist + washDist + IdealPointDistance + (1|region2), data = modData)
m2 = lmer(
  econDelta_lfm ~
    polity.l1 + GDP.l1 + pop.l1 +
    beijDist + washDist + IdealPointDistance + (1 + USf1.l1 |region2), data = modData)
m3 = lmer(
  econDelta_lfm ~
    polity.l1 + GDP.l1 + pop.l1 +
    beijDist + washDist + IdealPointDistance + (1 + USf2.l1 |region2), data = modData)
####

####
options(mc.cores = parallel::detectCores())
m1b = stan_lmer(
  econDelta_lfm ~
    USf1.l1 + polity.l1 + GDP.l1 + pop.l1 +
    beijDist + washDist + IdealPointDistance + (1|region2),
    data = modData, seed=6886)
m2b = stan_lmer(
  econDelta_lfm ~
    polity.l1 + GDP.l1 + pop.l1 +
    beijDist + washDist + IdealPointDistance + (1 + USf1.l1 |region2),
    data = modData, seed=6886)
m3b = stan_lmer(
  econDelta_lfm ~
    polity.l1 + GDP.l1 + pop.l1 +
    beijDist + washDist + IdealPointDistance + (1 + USf2.l1 |region2),
    data = modData, seed=6886)
####

####
# stdz all vars
modDataZ = modData
vars = c(
  # beijDist and washDist already scaled
  'econDelta_lfm', 'USf1.l1', 'polity.l1',
  'GDP.l1', 'pop.l1', 'IdealPointDistance')
for(v in vars){ modDataZ[,v] = scale(modDataZ[,v]) }

m1bZ = stan_lmer(
  econDelta_lfm ~
    USf1.l1 + polity.l1 + GDP.l1 + pop.l1 +
    beijDist + washDist + IdealPointDistance + (1|region2),
    data = modDataZ, seed=6886)
m2bZ = stan_lmer(
  econDelta_lfm ~
    polity.l1 + GDP.l1 + pop.l1 +
    beijDist + washDist + IdealPointDistance + (1 + USf1.l1 |region2),
    data = modDataZ, seed=6886)
m3bZ = stan_lmer(
  econDelta_lfm ~
    polity.l1 + GDP.l1 + pop.l1 +
    beijDist + washDist + IdealPointDistance + (1 + USf2.l1 |region2),
    data = modDataZ, seed=6886)
####

####
#
save(
  m1, m2, m3,
  m1b, m2b, m3b,
  m1bZ, m2bZ, m3bZ,
  modData, modDataZ,
  file=paste0(rpth, 'dstreamModels.rda')
)
####
