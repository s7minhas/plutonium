####
rm(list=ls())
pth = paste0(here::here(), '/')

#
source(paste0(pth, 'setup.R'))

#
loadPkg('cshapes')
####

####
load(paste0(pathIn, "dyadData.rda"))
####

####
# subset to china as being cname2
# idea here is that we want to measure the extent to
# which countries in the system are sending similar
# types of actions as china
### in the case of agree: what this means is that country i
### is likely to vote similarly to china
### this interpretation can be confirmed by taking the correlation
### between the u dist and v dist spaces, which in each case
### of the ame params that we tested hovered around 0.99
### in the case of trade: what this means is that country i
### is likely to send trade to the same countries that
### china is sending trade to
chiData = dyadData[dyadData$cname2 == "CHINA",]
chiData = chiData[chiData$cname1 != "CHINA",]
####

####
# merge in data on us distraction from scott
usLevel = read.csv(paste0(pathIn, 'main1_final.csv'))
usLevel = usLevel[order(usLevel$year),]

chiData$USf1 = usLevel$f1_USdeaths_MIDEAST[match(chiData$year, usLevel$year)]
chiData$USf2 = usLevel$f2_DEFspend_FORcommits[match(chiData$year, usLevel$year)]
chiData$USf3 = usLevel$f3_UE[match(chiData$year, usLevel$year)]
####

####
# merge in polity
polity = read.csv(paste0(pathIn, 'p5v2018.csv'))

# only keep countries in data that exist after 2000
pol00Cntries = unique(polity$country[polity$year>=2000])
polity = polity[polity$country %in% pol00Cntries,]

# use countrycode to convert to stdz name
polity$cname = cname(polity$country)

# fix mistakes
polity$cname[polity$country=='Congo Kinshasa'] = cname('Democratic Republic of Congo')

# create id for merging
polity$cnameYr = with(polity, paste0(cname, year))
toDrop = names(table(polity$cnameYr)[table(polity$cnameYr)>1])
polity = polity[!polity$cnameYr %in% toDrop,]

# merge with chiData
chiData$cnameYr = paste0(chiData$cname1, chiData$year)
chiData$polity = polity$polity2[match(chiData$cnameYr, polity$cnameYr)]
chiData$region = countrycode(chiData$cname1, "country.name", "region")
####

####
load(paste0(pathIn, 'wbData.rda'))
wbData$cnameYr = paste0(wbData$cname, wbData$year)
chiData$gdp = wbData$gdp[match(chiData$cnameYr, wbData$cnameYr)]
chiData$pop = wbData$pop[match(chiData$cnameYr, wbData$cnameYr)]
####

####
# prep data that will actually go into modeling

# separate out id variables
idVars = c('cname1','cowc1','cname2','cowc2','year','id')

# pull out variables for analysis
vars = c(
  'agree_k2_srm_lfm', 'agree_k5_srm_lfm',
  'tradeDepSend_k2_srm_lfm', 'tradeDepSend_k5_srm_lfm',
  "USf1", "USf2", "USf3","polity", "gdp", "pop" )

# create model frame
modData = chiData[,c(idVars, vars)]

# create lagged version
toMerge = modData
toMerge$year = toMerge$year + 1
toMerge$id = with(toMerge, paste(cowc1, cowc2, year, sep='_'))
names(toMerge)[(length(idVars)+1):ncol(toMerge)] = paste0(
  'lag1_', names(toMerge)[(length(idVars)+1):ncol(toMerge)] )

# merge lagged vars into modData
varsToMerge = paste0('lag1_', vars)
modData = cbind(modData, toMerge[match(modData$id, toMerge$id), varsToMerge])
####

####
# processing

# add region coding
modData$region = countrycode(modData$cname1, 'country.name', 'region')

# add region 23 from countrycode
load(paste0(pathIn, 'codelist.rda'))
codelist = codelist[!is.na(codelist$cow.name),]
codelist$cname = cname(codelist$cow.name)
modData$region23 = codelist$region23[match(modData$cname1, codelist$cname)]

# set up region codings
modData$chinaRegions = modData$region2 %in% c(
  "Eastern Asia", "Australia and New Zealand", "South-Eastern Asia")

# log vars
modData$lag1_gdp = log(modData$lag1_gdp + 1)
modData$lag1_pop = log(modData$lag1_pop + 1)

# lag vars and create diff version of dvs
dvs = c(
  'agree_k2_srm_lfm', 'agree_k5_srm_lfm',
  'tradeDepSend_k2_srm_lfm', 'tradeDepSend_k5_srm_lfm')
for(dv in dvs){
  modData$tmp = modData[,dv] - modData[,paste0('lag1_',dv)]
  names(modData)[ncol(modData)] = paste0('delta_',dv) }
####

####
# get geo info from cshapes
if(!file.exists(paste0(pathIn, 'geoInfo.rda'))){
  geoMin = distlist( as.Date('2019-12-31'),
    type = 'mindist' )
  geoCap = distlist( as.Date('2019-12-31'),
    type = 'capdist' )
  geoCent = distlist( as.Date('2019-12-31'),
    type = 'centdist' )
  save(geoMin, geoCap, geoCent, file=paste0(pathIn, 'geoInfo.rda'))
} else { load(paste0(pathIn, 'geoInfo.rda')) }

# org
geoData = cbind( geoMin, capdist=geoCap[,3], centdist=geoCent[,3])
geoData = geoData[geoData$ccode1 != geoData$ccode2,]

# subset to rows involving china
geoData = geoData[geoData$ccode1==710,]

# mod country labels
cntryKey = data.frame(cntry=geoData$ccode2, stringsAsFactors=F)
cntryKey$cname = cname(codelist$cow.name[match(cntryKey$cntry, codelist$gwn)])
cntryKey$cname[cntryKey$cntry==340] = 'SERBIA'
cntryKey$cname[cntryKey$cntry==816] = 'VIETNAM'
geoData$cname2 = cntryKey$cname[match(geoData$ccode2, cntryKey$cntry)]
geoData$cname1 = 'CHINA'

# merge
modData = cbind(modData, geoData[match(modData$cname1, geoData$cname2),3:5])
####

####
# natural security index ... average across indicators from 2013-2017
# https://www.newamerica.org/resource-security/reports/great-power-resource-competition-changing-climate/
natSec = read.csv(paste0(pathIn, 'natural_security_index.csv'), stringsAsFactors=F)
natSec$cname = cname(natSec$country)

# merge
vars = c(
  'crit_cn', 'crit_us', 'metals_cn', 'metals_us',
  'ag_cn', 'ag_us', 'energy_cn', 'energy_us',
  'resources_cn', 'resources_us')
modData = cbind(modData, natSec[match(modData$cname1,natSec$cname),vars])
####

####
# add categorical version of vars that will be used for REs

# create pol cat var
modData$polCat3 = cut(modData$polity,
  breaks=c(-Inf, -6, 5, Inf), lables=c('Autocracy','Anocracy', 'Democracy'))

# create categories based on qt calcs
modData$polCatLo = recodeQt(modData$polity, seq(0,1,.25))
modData$polCatHi = recodeQt(modData$polity, seq(0,1,.1))

# create categoricals from
modData$minChinaCatLo = recodeQt(modData$crit_cn, seq(0,1,.25))
modData$minChinaCatHi = recodeQt(modData$crit_cn, seq(0,1,.1))
modData$metalsChinaCatLo = recodeQt(modData$metals_cn, seq(0,1,.25))
modData$metalsChinaCatHi = recodeQt(modData$metals_cn, seq(0,1,.1))
####

####
# restrict range
modData = modData[modData$year>=2000,]
####

####
save(modData, file = paste0(pathIn, "modData.rda"))
####
