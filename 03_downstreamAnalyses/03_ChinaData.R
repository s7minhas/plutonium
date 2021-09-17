####
rm(list=ls())
pth = paste0(here::here(), '/')
dapth = paste0(pth, '03_downstreamAnalyses/')

#
source(paste0(pth, 'setup.R'))
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
toLag = c("cname1", "year", "diplomScores_agree_lfm", "econScores_tradeDepSend_lfm", "icewsScores_gov_lfm", "USf1", "USf2", "USf3", "polity", "GDP", "pop")
ld = chiData[,toLag]
lags = multilagger(ld, ld$cname1, ld$year, laglength = 1)
chiData = cbind(chiData, lags[,3:dim(lags)[2]])
####

####
# add categorical version of polity
chiData$polCat3 = 'Anocracy'
chiData$polCat3[which(chiData$polity <= -6)] = 'Autocracy'
chiData$polCat3[which(chiData$polity >= 6)] = 'Democracy'


####

####
save(chiData, file = paste0(pathIn, "chiData.rda"))
####
