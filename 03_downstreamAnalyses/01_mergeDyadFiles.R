####
rm(list=ls())
pth = paste0(here::here(), '/')
source(paste0(pth, 'setup.R'))

# load data
dpth = paste0(pth, 'data/')
# dpth = "/Users/maxgallop/Dropbox/booz_allen_gdmm/data/"
load(paste0(dpth, 'frame.rda'))
####

####
# merge in scores into frame
fpth = paste0(pth, 'results/')
# fpth = "/Users/maxgallop/Dropbox/booz_allen_gdmm/results"
files = c(
	'mltrScores_ML.rda',
	'mltrScores.rda',
	'lfmScores.rda' )

# iter through and merge
for(fname in files){

	# load
	tmp=ls()
	load(paste0(fpth, fname))
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
# pth for downstream folder
dapth = paste0(pth, '03_downstreamAnalyses/')
dapth = "/Users/maxgallop/Dropbox/booz_allen_gdmm/03_downstreamAnalyses"
# save
dyadData = frame
save(dyadData, file=paste0(dapth, 'dyadData.rda'))
####
