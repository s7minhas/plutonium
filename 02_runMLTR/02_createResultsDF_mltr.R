####
rm(list=ls())
pth = paste0(here::here(), '/')
source(paste0(pth, 'setup.R'))

# load data
dpth = paste0(pth, 'data/')
load(paste0(dpth, 'frame.rda'))
frame = frame[,1:4]
####

####
# fn to extract scores from mltr
processMLTR = function(
	yrs, fname, fpth=paste0(pth, 'results/') ){

	# load object and reassign
	# its name to mod
	tmp=ls()
	load(paste0(fpth, fname))
	objName = setdiff(ls(),c(tmp,'tmp'))
	assign("mod", get(objName))
	rm(list=c(objName))

	# iterate through years and extract mean
	means = lapply(1:length(mod), function(ii){

		# burn first third of chain then
		# calculate mean
		modSlice = mod[[ii]]
		muMat = apply(
			modSlice[,,-(1:round(dim(modSlice)[3]/3,0))],
			c(1,2), mean)

		# organize into df
		muDF = reshape2::melt(muMat)
		names(muDF) = c('Var1', 'Var2', 'value')
		muDF = muDF[muDF$Var1 != muDF$Var2,]
		muDF$year = yrs[ii]
		return(muDF) })

	# combine means into a single df
	means = do.call('rbind', means)

	# replace varname
	names(means)[3] = gsub('.rda','',fname)

	# create id var
	means$id = with(means,
		paste(Var1, Var2, year, sep='_'))

	#
	return(means) }
####

####
# apply process mltr fn
diplomScoresAgree = processMLTR(
	yrs=1980:2019,
	fname='diplomScores_agree.rda')

#
econScoresTradeDep = processMLTR(
	yrs=1995:2020,
	fname='econScores_tradeDepSend.rda')

#
icewsScoresGov = processMLTR(
	yrs=2000:2020,
	fname='icewsScores_gov.rda')
####

####
# organize
toMerge = list(
	diplomScoresAgree,
	econScoresTradeDep,
	icewsScoresGov)

# merge
for(df in toMerge){
	var=names(df)[3]
	frame$tmp = df[match(frame$id, df$id),var]
	names(frame)[ncol(frame)] = var }
####

####
#
mltrScores = frame
save(mltrScores,
	file=paste0(pth, 'results/mltrScores.rda') )
####
