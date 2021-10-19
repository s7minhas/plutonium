# prep data for net analysis
####
rm(list=ls())
pth = paste0(here::here(), '/')
source(paste0(pth, 'setup.R'))
load(paste0(pathIn, 'samp.rda'))
####

####
# create sample frame to which
# each dataset will be merged
frame = do.call('rbind', lapply(
	1975:2020, function(t){
		slice = panel[panel$year==t,]
		dyad = expand.grid(
			cname1=slice$cname,
			cname2=slice$cname,
			year=t, stringsAsFactors=FALSE )
		dyad = dyad[dyad$cname1!=dyad$cname2,]
		return(dyad) } ))
frame$id = with(frame, paste(cname1, cname2, year, sep='_'))

# merge in data
fileShorts = c(
		'atop', 'desta',
		'icewsQuadGov', 'icewsQuad',
		'idPt', 'imfData' )

# iterate through files and merge
# into frame
for(f in fileShorts){
	load(paste0(pathIn, f, '.rda'))
	assign('toMerge', get(f)) ; rm(f)

	# merge all non-id vars from dataset
	vars = names(toMerge)[5:ncol(toMerge)]
	frame = cbind(
		frame,
		toMerge[match(frame$id, toMerge$id),vars] ) }

# also load in nodal economic information from WDI
# and merge
load(paste0(pathIn, 'wbData.rda'))
wbVars = names(wbData)[4:(ncol(wbData)-1)]
for(v in wbVars){

	# add in sender side info
	frame$tmp = wbData[match(frame$cname1,wbData$cname),v]
	names(frame)[ncol(frame)] = paste0(v, '_i')

	# add in receiver side info
	frame$tmp = wbData[match(frame$cname2,wbData$cname),v]
	names(frame)[ncol(frame)] = paste0(v, '_j') }
####

####
# cleanup NAs where relevant
# atop ends at 2018, but we extended to 2020, assuming 2020, 2019 vals = 2018
# idPt ends at 2019
# desta ends at 2020
# icews starts at 1995 and ends at 2020
# imf stats at 1990 and ends at 2020

# pull out vector of vars for which NAs can be replaced with zero,
# with imfData ideally we'd impute missingness in the long run,
# since there are issues with setting the NAs to zero universally
# idPt vars should not be set to NA as the missings there indicate
# times in which a country was essentially not in the UN
eventVars = setdiff(
	names(frame),
	c(
		'agree', 'IdealPointDistance',
		pasteVec(wbVars, c('_i','_j')),
		names(frame)[1:4]) )
frame[,eventVars] = apply(
	frame[,eventVars], 2, function(x){
		x[is.na(x)] = 0 ; return(x) } )

# make sure idPt vars are NA after 2019
# note that for ally we replicate 2018 through 2020
naVars = c(
	'IdealPointDistance', 'agree' )
frame[frame$year==2020,naVars] = NA
####

####
# icews specific prep
icewsVars = pasteVec(
	c('matl','verb'), c('Conf', 'Coop')) %>%
	pasteVec(., c('', 'Gov') )

# set as NA before 1995
frame[frame$year<1995,icewsVars] = NA

# log each of the vars
frame[,icewsVars] = log( frame[,icewsVars] + 1 )

# flip scale of conflictual variables
toFlip = pasteVec(c('matlConf','verbConf'),c('','Gov'))
frame[,toFlip] = frame[,toFlip]*-1
####

####
# imf specific prep
imfVars = pasteVec(c('imports','exports'),c('CIF','FOB'))

# set as NA before 1990
frame[frame$year<1990,imfVars] = NA

# create total trade variable
frame$trade = with(frame, importsCIF + exportsFOB)

# calc level of trade dependence
tradeTot = frame %>%
	group_by(cname1, year) %>%
	summarize(
		tradeSum = sum(trade, na.rm=TRUE) )

# merge in sender level trade calc into frame
frame$tradeSend = tradeTot$tradeSum[match(
	paste(frame$cname1, frame$year),
	paste(tradeTot$cname1, tradeTot$year) )]
frame$tradeSend[frame$year<1990] = NA

# calc level of trade dependence from sender
# perspective, set NAs to zero as these are from
# zeros in the denom
frame$tradeDepSend = with(frame, trade/tradeSend)
frame$tradeDepSend[is.na(frame$tradeDepSend)] = 0
frame$tradeDepSend[frame$year<1990] = NA

# calc level of trade dependence using gdp of sender as denom
frame$tradeGDP = with(frame, trade/gdp_i)

# logged version of trade
# skip logging since we're standardizing,
# logging and stdizing seems to compress
# diffs too much
# frame$trade = log(frame$trade + 1)
####

####
# stdz various relational measures by year

# save versions of a few vars so that we
# add some raw versions back in for testing
tradeRaw = frame$trade
tradeDepSendRaw = frame$tradeDepSend
tradeGDPRaw = frame$tradeGDP
allyTotal = frame$allyTotal
ptaCnt = frame$ptaCnt

# stdz by year
yrs = sort(unique(frame$year))
for(t in yrs){
	slice = frame[frame$year==t,5:ncol(frame)]
	slice = apply(slice, 2, stdz)
	frame[frame$year==t,5:ncol(frame)] = slice }

# merge back in raw trade val
frame$tradeRaw = tradeRaw ; rm(tradeRaw)
frame$tradeDepSendRaw = tradeDepSendRaw ; rm(tradeDepSendRaw)
frame$tradeGDPRaw = tradeGDPRaw ; rm(tradeGDPRaw)
frame$allyTotalRaw = allyTotal ; rm(allyTotal)
frame$ptaCntRaw = ptaCnt ; rm(ptaCnt)

# add in gen coop index
frame$treatyCoopRaw = frame$ptaCntRaw + frame$allyTotalRaw
frame$treatyCoopZ = stdz(frame$treatyCoop)
frame$treatyCoopBin = 1*(frame$treatyCoopRaw>0)
frame$ptaBin = 1*(frame$ptaCnt>0)
frame$allyBin = 1*(frame$allyTotal>0)
####

####
# replace NAs in idealpointdistance and agree with zero
# which will essentially make it so that some dyads dont
# contribute to the likelihood, NAs in these vars arise
# mostly because a country was not in the UN, imputation
# would not be a good option for this types of NA
frame$IdealPointDistance[frame$year<2020 & is.na(frame$IdealPointDistance)] = 0
frame$agree[frame$year<2020 & is.na(frame$agree)] = 0
####

####
save(
	frame, file=paste0(pathIn, 'frame.rda')
)
####
