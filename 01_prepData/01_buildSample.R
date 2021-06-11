# build sample
####
rm(list=ls())
pth = paste0(here::here(), '/')
dpth = paste0(pth, 'data/')
source(paste0(pth, 'setup.R'))
load(paste0(dpth, 'baseSamp.rda'))
####

####
polity = read.csv(paste0(dpth, 'p5v2018.csv'))
polity = polity[polity$year>1990,]
polity$cname = countrycode(polity$country, 'country.name','country.name')
polity$cnameYr = with(polity, paste0(cname, year))
toDrop = names(table(polity$cnameYr)[table(polity$cnameYr)>1])
polity = polity[!polity$cnameYr %in% toDrop,]
toKeep = unique(polity$cname)

samp = samp[samp$cname %in% toKeep,]
####

####
if(!file.exists(paste0(dpth, 'wbData.rda'))){
library(WDI)
wbData <- WDI(
  country="all",
  indicator=c(
    'NY.GDP.MKTP.CD',
    'SP.POP.TOTL'
  ),
  start=1991,
  end=2020,
  extra=TRUE
)
save(wbData, file=paste0(dpth, 'wbData.rda'))
} else { load(paste0(dpth, 'wbData.rda')) }

wbData$cname = countrycode(
	wbData$country, 'country.name',
	'country.name')
wbData = wbData[!is.na(wbData$cname),]

popThresh = wbData %>%
	group_by(cname) %>%
	summarize(
		min = min(SP.POP.TOTL, na.rm=TRUE) )
popThresh = popThresh[popThresh$min>=1000000,]
toKeep = popThresh$cname

samp = samp[which(samp$cname %in% toKeep),]
####

####
save(
	samp,
	file=paste0(dpth, 'samp.rda')
)
####
