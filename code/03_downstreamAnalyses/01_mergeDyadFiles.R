####
rm(list=ls())
pth = paste0(here::here(), '/')
source(paste0(pth, 'setup.R'))

#
loadPkg('tidyr')

# load data
load(paste0(pathIn, 'frame.rda'))
####

####
# merge in scores into frame
files = c(
	'mltrScores_ML.rda',
	'mltrScores.rda',
	'lfmScores.rda' )

# iter through and merge
for(fname in files){

	# load
	tmp=ls()
	load(paste0(pathOut, fname))
	objName = setdiff(ls(),c(tmp,'tmp'))
	assign("modData", get(objName))
	rm(list=c(objName))

	# find vars that are not already in frame
	toMerge = setdiff(names(modData), names(frame))

	# bind those vars in via id
	frame = cbind(
		frame,
		modData[
			match(frame$id, modData$id), toMerge] ) }
####

####
cntryKey = data.frame(dirty=unique(c(frame$cname1, frame$cname2)), stringsAsFactors=FALSE)
cntryKey$clean = countrycode(cntryKey$dirty, 'country.name', 'cown')
frame$ccode1 = cntryKey$clean[match(frame$cname1, cntryKey$dirty)]
frame$ccode2 = cntryKey$clean[match(frame$cname2, cntryKey$dirty)]
frame$dyad = paste(frame$ccode1, frame$ccode2, sep ="_")
####

####
# load in u, v dist metrics
load(file=paste0(pathOut, 'unDist.rda'))
load(file=paste0(pathOut, 'tradeDist.rda'))

#
## k5 k2
## tradeDepSend agree

# subset to relevant vars for UN
unVars = paste0('agree_k', c(2,5), '_srm_lfm')
unDist = unDist[unDist$config %in% unVars,]
unDist = unDist[unDist$param=='U' & unDist$dist=='cosine',]
unDist = pivot_wider(unDist, names_from=config, values_from=value)

# subset to relevant vars for trade
tradeVars = paste0('tradeDepSend_k', c(2,5), '_srm_lfm')
tradeDist = tradeDist[tradeDist$config %in% tradeVars,]
tradeDist = tradeDist[tradeDist$param=='U' & tradeDist$dist=='cosine',]
tradeDist = pivot_wider(tradeDist, names_from=config, values_from=value)

# add abbreviations to frame so we can merge
cntryKey = data.frame(cntry=unique(c(frame$cname1, frame$cname2)), stringsAsFactors=FALSE)
cntryKey$cowc = countrycode(cntryKey$cntry, 'country.name', 'cowc')
cntryKey$cowc[cntryKey$cntry=='SERBIA'] = 'YUG'
frame$cowc1 = cntryKey$cowc[match(frame$cname1, cntryKey$cntry)]
frame$cowc2 = cntryKey$cowc[match(frame$cname2, cntryKey$cntry)]

# create id vars in each for merging
unDist$id = with(unDist, paste(Var1, Var2, year, sep='_'))
tradeDist$id = with(tradeDist, paste(Var1, Var2, year, sep='_'))
frame$id = with(frame, paste(cowc1, cowc2, year, sep='_'))

# merge in unDist and tradeDist
frame = cbind(frame, unDist[match(frame$id, unDist$id),unVars])
frame = cbind(frame, tradeDist[match(frame$id, tradeDist$id),tradeVars])
####


####
# save
dyadData = frame
save(dyadData, file=paste0(pathIn, 'dyadData.rda'))
####
