####
rm(list=ls())
pth = paste0(here::here(), '/')
source(paste0(pth, 'setup.R'))

#
loadPkg('haven')
####

####
dat = read_dta(
  paste0(pathGit, 'code/03_downstreamAnalyses/data_us_commit/main_constraint_1.dta'))
dat = data.frame(dat, stringsAsFactors=F)
####

####
vars = c(
  'def_per_gdp',
  'asia_per',
  'westerneurope_per',
  'easterneurope_per',
  'formersovietunion_per',
  'middleeast_per',
  'africa_per',
  'perforeignmil',
  'f_l_stock_crises',
  'f_l_price_crises',
  'emp_rate',
  'gdp_growth',
  'std_defense_budget',
  'std_totalworld',
  'std_totalformil',
  'std_hostiledeaths' )

#
fData = dat[,vars]
fData = apply(fData, 2, function(x){ (x-mean(x))/sd(x) })

#
faMod = factanal(
  fData, factors=3, lower=0.01, scores='regression')

#
dat = cbind(dat, faMod$scores)
####

####
#
slice = dat[,c(
  'f1_USdeaths_MIDEAST',
  'f2_DEFspend_FORcommits',
  'f3_UE',
  'Factor1', 'Factor2', 'Factor3' )]

round(cor(slice), 2)
head(slice)
####
