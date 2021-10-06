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
load(paste0(dpth, "chiData_v2.rda"))
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
    USf1.l1 + GDP.l1 + pop.l1 +
    beijDist + IdealPointDistance + (1|polity), data = modData)
mf2 = lmer(
  econDelta_lfm ~
    USf2.l1 + GDP.l1 + pop.l1 +
    beijDist + IdealPointDistance + (1|polity), data = modData)
mf1HetEff = lmer(
  econDelta_lfm ~
    GDP.l1 + pop.l1 +
    beijDist + IdealPointDistance + (1 + USf1.l1 |polity), data = modData)
mf2HetEff = lmer(
  econDelta_lfm ~
    GDP.l1 + pop.l1 +
    beijDist + IdealPointDistance + (1 + USf2.l1 |polity), data = modData)
####

summary(mf1)
summary(mf2)
summary(mf1HetEff)
summary(mf2HetEff)


####
# default four chains and 2000 iterations

options(mc.cores = parallel::detectCores())
bf1 = stan_lmer(
  econDelta_lfm ~
    USf1.l1 + GDP.l1 + pop.l1 +
    beijDist + IdealPointDistance + (1|polity),
    chain = 10, iter=8000,
    data = modData, seed=6886)
bf2 = stan_lmer(
  econDelta_lfm ~
    USf2.l1 + GDP.l1 + pop.l1 +
    beijDist + IdealPointDistance + (1|polity),
    chain = 10, iter=8000,
    data = modData, seed=6886)
bf1HetEff = stan_lmer(
  econDelta_lfm ~
    GDP.l1 + pop.l1 +
    beijDist + IdealPointDistance + (1 + USf1.l1 |polity),
    chain = 10, iter=8000,
    data = modData, seed=6886)
bf2HetEff = stan_lmer(
  econDelta_lfm ~
    GDP.l1 + pop.l1 +
    beijDist + IdealPointDistance + (1 + USf2.l1 |polity),
    chain = 10, iter=8000,
    data = modData, seed=6886)
####

summary(bf1)
summary(bf2)
summary(bf1HetEff)
summary(bf2HetEff)

plot(bf1)

####
#
save(
  mf1, mf2, mf1HetEff, mf2HetEff,
  bf1, bf2, bf1HetEff, bf2HetEff,
  modData,
  file=paste0(rpth, 'dstreamModels_polCat3.rda')
)
####

summary(bf1)
summary(bf2)
summary(bf1HetEff)
summary(bf2HetEff)
