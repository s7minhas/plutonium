####
rm(list=ls())
pth = paste0(here::here(), '/')
source(paste0(pth, 'setup.R'))

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
# add ccodes and dyad var
frame$ccode1 = countrycode(frame$cname1, "country.name", "cown")
frame$ccode2 = countrycode(frame$cname2, "country.name", "cown")
frame$dyad = paste(frame$ccode1, frame$ccode2, sep ="_")
####

####
# save
dyadData = frame
save(dyadData, file=paste0(pathIn, 'dyadData.rda'))
####
