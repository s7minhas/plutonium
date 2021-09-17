####
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


####

####
# relabel as modData so we can save with models
modData = chiData
####

####
# f1 us deaths mideast
# f2 us def spend
# econDelta_lfm: higher values means that they are more likely to economic ties
mf1 = lmer(
  econDelta_lfm ~
    USf1.l1 + polity.l1 + GDP.l1 + pop.l1 +
    beijDist + IdealPointDistance + (1|cname1), data = modData)
mf2 = lmer(
  econDelta_lfm ~
    USf2.l1 + polity.l1 + GDP.l1 + pop.l1 +
    beijDist + IdealPointDistance + (1|cname1), data = modData)
mf1HetEff = lmer(
  econDelta_lfm ~
    polity.l1 + GDP.l1 + pop.l1 +
    beijDist + IdealPointDistance + (1 + USf1.l1 |cname1), data = modData)
mf2HetEff = lmer(
  econDelta_lfm ~
    polity.l1 + GDP.l1 + pop.l1 +
    beijDist + IdealPointDistance + (1 + USf2.l1 |cname1), data = modData)
####

####
options(mc.cores = parallel::detectCores())
bf1 = stan_lmer(
  econDelta_lfm ~
    USf1.l1 + polity.l1 + GDP.l1 + pop.l1 +
    beijDist + IdealPointDistance + (1|cname1),
    data = modData, seed=6886)
bf2 = stan_lmer(
  econDelta_lfm ~
    USf2.l1 + polity.l1 + GDP.l1 + pop.l1 +
    beijDist + IdealPointDistance + (1|cname1),
    data = modData, seed=6886)
bf1HetEff = stan_lmer(
  econDelta_lfm ~
    polity.l1 + GDP.l1 + pop.l1 +
    beijDist + IdealPointDistance + (1 + USf1.l1 |cname1),
    data = modData, seed=6886)
bf2HetEff = stan_lmer(
  econDelta_lfm ~
    polity.l1 + GDP.l1 + pop.l1 +
    beijDist + IdealPointDistance + (1 + USf2.l1 |cname1),
    data = modData, seed=6886)
####

####
# gut check: non-homogenous effects
# basically see if those countries that are closest to china respond
# to changes in us distraction
chiData$chinaRegions = chiData$region2 %in% c(
  "Eastern Asia", "Australia and New Zealand", "South-Eastern Asia")
cInter2 = lm(
  econDelta_lfm ~
    USf1.l1*chinaRegions + polity.l1+GDP.l1+ pop.l1 +
    beijDist + IdealPointDistance + region2, data = chiData)
summary(cInter2)
####

####
#
save(
  mf1, mf2, mf1HetEff, mf2HetEff,
  bf1, bf2, bf1HetEff, bf2HetEff,
  modData,
  file=paste0(rpth, 'dstreamModels_cname.rda')
)
####
