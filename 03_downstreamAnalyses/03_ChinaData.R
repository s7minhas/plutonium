
dapth = "/Users/maxgallop/Dropbox/booz_allen_gdmm/03_downstreamAnalyses/"

load(paste0(dapth, "dyadData.rda"))


chiData = dyadData[dyadData$cname2 == "CHINA",]
chiData = chiData[chiData$cname1 != "CHINA",]

usLevel = read.csv(paste0(dapth, '/main1_final.csv'))
usLevel = usLevel[order(usLevel$year),]

chiData$USf1 = usLevel$f1_USdeaths_MIDEAST[match(chiData$year, usLevel$year)]
chiData$USf2 = usLevel$f2_DEFspend_FORcommits[match(chiData$year, usLevel$year)]
chiData$USf3 = usLevel$f3_UE[match(chiData$year, usLevel$year)]

###FixefWant: Polity, Distance to China/US, Region, GDP, Population
###RandomEffs: Region

dpth = "/Users/maxgallop/Dropbox/booz_allen_gdmm/data/"
polity = read.csv(paste0(dpth, 'p5v2018.csv'))

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


bad = unique(chiData$cname1[is.na(chiData$polity)])
good = unique(chiData$cname1[!is.na(chiData$polity)])
needfix = setdiff(bad, good)

chiData$polity[chiData$cname1 %in% needfix] = polity$polity2[match(chiData$codeyr[chiData$cname1 %in% needfix], toupper(polity$codeyr))]

load(paste0(dpth, 'wbData.rda'))
wbData$cnameYr = paste0(wbData$cname, wbData$year)
chiData$GDP = wbData$NY.GDP.MKTP.CD[match(chiData$cnameYr, wbData$cnameYr)]
chiData$pop = wbData$SP.POP.TOTL[match(chiData$cnameYr, wbData$cnameYr)]

chiData$gwcode1 = countrycode(chiData$cname1, "country.name", "gwn")
chiData$date = as.Date(paste0("1/1/", chiData$year), format = "%m/%d/%Y")

years = 1990:2020
for(y in years){
  date = as.Date(paste0("1/1/", y), format = "%m/%d/%Y")
  use = distlist(date, useGW = T, type = "capdist")
  bd = use[use$ccode2 == 710,]
  ud = use[use$ccode2 == 2,]
  chiData$beijDist[chiData$year == y] = bd$capdist[match(chiData$gwcode1[chiData$year == y], bd$ccode1)]
  chiData$washDist[chiData$year == y] = ud$capdist[match(chiData$gwcode1[chiData$year == y], ud$ccode1)]
}

lagger<-function(variable, country, year, laglength){
  
  country<-as.character(country)
  laggedvar<-rep(NA,length(variable))
  
  leadingNAs<-rep(NA,laglength)
  countryshift<-c(leadingNAs, country[1:(length(country)-laglength)])
  
  variableshift<-c(leadingNAs, variable[1:(length(variable)-laglength)])
  
  replacementrefs<-country==countryshift
  replacementrefs[is.na(replacementrefs)==T]<-FALSE
  laggedvar[replacementrefs]<-variableshift[replacementrefs]
  
  laggedvar
  
} # close lagger function

# function for lagging whole dataframes:
multilagger<-function(X, country, year, laglength, relabel=T){
  
  if(is.data.frame(X)==F) stop("X needs to be a dataframe")
  
  laggedX<-X
  
  for (i in 1:ncol(X)){
    
    laggedX[,i]<-lagger(variable=X[,i], country=country, year=year, laglength=laglength)
    
  } # close i loop
  
  # now append the laglength to the variable names:
  if (relabel==T){
    suffix<-paste(".l",laglength,sep="")
    names(laggedX)<-paste(names(X), suffix, sep="")
  } # close if relabel==T condition
  
  laggedX
  
} # close multilagger function


names(chiData)
chiData = chiData[order(chiData$cname1, chiData$year),]
toLag = c("cname1", "year", "diplomScores_agree_lfm", "econScores_tradeDepSend_lfm", "icewsScores_gov_lfm", "USf1", "USf2", "USf3", "polity", "GDP", "pop")
ld = chiData[,toLag]
lags = multilagger(ld, ld$cname1, ld$year, laglength = 1)
chiData = cbind(chiData, lags[,3:dim(lags)[2]])

setwd("/Users/maxgallop/Dropbox/plutonium/data/")
save(chiData, file = "chiData.rda")
