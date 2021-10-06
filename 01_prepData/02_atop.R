####
rm(list=ls())
pth = paste0(here::here(), '/')
source(paste0(pth, 'setup.R'))
load(paste0(pathIn, 'samp.rda'))
####

####
# load atop dataset
atop = read.csv(
	paste0(pathIn, '/ATOP 5_0 (csv)/atop5_0ddyr.csv'))

# post 1975
atop = atop[atop$year>=1975,]

# cntries
cntryKey = data.frame(
	cowcode=sort(unique(c(atop$stateA, atop$stateB))),
	stringsAsFactors = FALSE )
cntryKey$cname = countrycode(
	cntryKey$cowcode, 'cown', 'country.name')
cntryKey$cname[cntryKey$cowcode==731] = cname('North Korea')
cntryKey$cname[cntryKey$cowcode==345] = 'SERBIA'

# add cnames
atop$cname1 = cntryKey$cname[match(atop$stateA, cntryKey$cowcode)]
atop$cname2 = cntryKey$cname[match(atop$stateB, cntryKey$cowcode)]
atop$id = with(atop, paste(cname1, cname2, year, sep='_'))

# subset to countries in samp
atop = atop[atop$cname1 %in% samp$cname,]
atop = atop[atop$cname2 %in% samp$cname,]
####

####
# filter to relev vars
ids = c('cname1', 'cname2', 'year', 'id')
vars = c(
	'defense', 'offense',
	'neutral', 'nonagg', 'consul')

atop = atop[,c(ids,vars)]
atop$allyTotal = apply(
	atop[,vars], 1, function(x){ sum(x) } )
####

####
# extend atop by one year
# assuming that alliances in 2019 are the same as those in 2018
tmp = atop[atop$year==2018,]

# repeat 2018 as 2019
tmp$year = 2019
tmp$id = with(tmp, paste(cname1, cname2, year, sep='_'))
atop = rbind(atop, tmp)

# repeat 2018 as 2020
tmp$year = 2020
tmp$id = with(tmp, paste(cname1, cname2, year, sep='_'))
atop = rbind(atop, tmp)
####

atop[atop$cname1=='UNITED STATES' & atop$cname2=='UNITED KINGDOM',]

atop[atop$offense==1,]

####
# Save to binaries
save(atop, file=paste0(pathIn,'atop.rda'))
####
