# get world bank data
####
rm(list=ls())
source(paste0(here::here(), '/setup.R'))
load(paste0(pathIn, 'samp.rda'))

#
loadPkg('WDI')
####

####
# pull world bank data from wdi
wbData = WDI(
	indicator = c(
		'SP.POP.TOTL', # population
		'ny.gdp.mktp.cd' # gdp
		),
	start=2000, end=2020 )

# clean up names
wbData$cname = countrycode(wbData$iso2c, 'iso2c', 'country.name')
wbData = wbData[!is.na(wbData$cname),]
names(wbData)[4:5] = c('pop', 'gdp')

# limit to countries in sample
wbData = wbData[wbData$cname %in% samp$cname,]
####

####
save(wbData, file=paste0(pathIn, 'wbData.rda'))
####
