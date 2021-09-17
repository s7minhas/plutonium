####
rm(list=ls())
pth = paste0(here::here(), '/')
dapth = paste0(pth, '03_downstreamAnalyses/')

#
source(paste0(pth, 'setup.R'))

#
loadPkg('cshapes')
####

####
load(paste0(pathIn, "dyadData.rda"))
####

####
chiData = dyadData[dyadData$cname2 == "CHINA",]
chiData = chiData[chiData$cname1 != "CHINA",]

usLevel = read.csv(paste0(pathIn, 'main1_final.csv'))
usLevel = usLevel[order(usLevel$year),]

chiData$USf1 = usLevel$f1_USdeaths_MIDEAST[match(chiData$year, usLevel$year)]
chiData$USf2 = usLevel$f2_DEFspend_FORcommits[match(chiData$year, usLevel$year)]
chiData$USf3 = usLevel$f3_UE[match(chiData$year, usLevel$year)]
####

####
polity = read.csv(paste0(pathIn, 'p5v2018.csv'))
polity$cname = countrycode(polity$country, 'country.name','country.name')
polity$cnameYr = with(polity, paste0(cname, year))
toDrop = names(table(polity$cnameYr)[table(polity$cnameYr)>1])
polity = polity[!polity$cnameYr %in% toDrop,]

polity$codeyr = with(polity, paste0(ccode, year))
chiData$ccode1 = countrycode(chiData$cname1, "country.name", "cown")
chiData$codeyr = with(chiData, paste0(ccode1, year))


chiData$cnameYr = paste0(chiData$cname1, chiData$year)
chiData$polity = polity$polity2[match(chiData$cnameYr, toupper(polity$cnameYr))]
chiData$region = countrycode(chiData$cname1, "country.name", "region")
####

####
load(paste0(pathIn, 'wbData.rda'))
wbData$cnameYr = paste0(wbData$cname, wbData$year)
chiData$GDP = wbData$NY.GDP.MKTP.CD[match(chiData$cnameYr, wbData$cnameYr)]
chiData$pop = wbData$SP.POP.TOTL[match(chiData$cnameYr, wbData$cnameYr)]
####

####
chiData$gwcode1 = countrycode(chiData$cname1, "country.name", "gwn")
chiData$date = as.Date(paste0("1/1/", chiData$year), format = "%m/%d/%Y")
####

####
years = 1990:2020
for(y in years){
  date = as.Date(paste0("1/1/", y), format = "%m/%d/%Y")
  use = distlist(date, useGW = T, type = "capdist")
  bd = use[use$ccode2 == 710,]
  ud = use[use$ccode2 == 2,]
  chiData$beijDist[chiData$year == y] = bd$capdist[match(chiData$gwcode1[chiData$year == y], bd$ccode1)]
  chiData$washDist[chiData$year == y] = ud$capdist[match(chiData$gwcode1[chiData$year == y], ud$ccode1)]
}
####

####
chiData = chiData[order(chiData$cname1, chiData$year),]
toLag = c(
  "cname1", "year",
  "diplomScores_agree_lfm", "econScores_tradeDepSend_lfm",
  "icewsScores_gov_lfm",
  "USf1", "USf2", "USf3",
  "polity", "GDP", "pop")
ld = chiData[,toLag]
lags = multilagger(ld, ld$cname1, ld$year, laglength = 1)
chiData = cbind(chiData, lags[,3:dim(lags)[2]])
####

load(paste0(pathIn, "chiData.rda"))

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
chiData$region3 = countrycode(chiData$cname1, "country.name", "region")
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
# add categorical version of polity
chiData$polCat3 = 'Anocracy'
chiData$polCat3[which(chiData$polity <= -6)] = 'Autocracy'
chiData$polCat3[which(chiData$polity >= 6)] = 'Democracy'
####

####
# add categorical version of ideal point distance
qtVals = quantile(chiData$IdealPointDistance, probs=seq(0,1,.25), na.rm=TRUE)
chiData$idPt4 = 'qt0_25'
chiData$idPt4[
  which(
  chiData$IdealPointDistance > qtVals[2] &
  chiData$IdealPointDistance <= qtVals[3] )] = 'qt26_50'
chiData$idPt4[
  which(
  chiData$IdealPointDistance > qtVals[3] &
  chiData$IdealPointDistance <= qtVals[4] )] = 'qt51_75'
chiData$idPt4[
  which(
  chiData$IdealPointDistance > qtVals[4] )] = 'qt76_100'
####

####
save(chiData, file = paste0(pathIn, "chiData_v2.rda"))
####
