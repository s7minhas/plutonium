# get imf dots
####
rm(list=ls())
pth = paste0(here::here(), '/')
source(paste0(pth, 'setup.R'))
load(paste0(pathIn, 'samp.rda'))

#
loadPkg(c('imfr','foreach','doParallel'))
####

####
# get imf data via imfr

# cntries for which we want data in iso format
cntries = unique(samp$iso)

# imports cif
if(!file.exists(paste0(dpth, 'imfImpCif_v2.rda'))){
impCifL = lapply(cntries, function(cntry){
	out <- imf_data(
	  database_id = 'DOT',
	  indicator = "TMG_CIF_USD",
	  country = cntry, return_raw = TRUE,
	  start=1975, end=current_year(),
	  freq='A' )
	return(out)
})
save(impCifL, file=paste0(dpth, 'imfImpCif_v2.rda'))
} else { load(paste0(dpth, 'imfImpCif_v2.rda')) }

# exports fob
if(!file.exists(paste0(dpth, 'imfExpFob_v2.rda'))){
expFobL = lapply(cntries, function(cntry){
	out <- imf_data(
	  database_id = 'DOT',
	  indicator = "TXG_FOB_USD",
	  country = cntry, return_raw = TRUE,
	  start=1975, end=current_year(),
	  freq='A' )
	return(out)
})
save(expFobL, file=paste0(dpth, 'imfExpFob_v2.rda'))
} else { load(paste0(dpth, 'imfExpFob_v2.rda')) }

# trade balance
if(!file.exists(paste0(dpth, 'imfTradeBal_v2.rda'))){
tradeBal = lapply(cntries, function(cntry){
	out <- imf_data(
	  database_id = 'DOT',
	  indicator = "TBG_USD",
	  country = cntry, return_raw = TRUE,
	  start=1975, end=current_year(),
	  freq='A' )
	return(out)
})
save(tradeBal, file=paste0(dpth, 'imfTradeBal_v2.rda'))
} else { load(paste0(dpth, 'imfTradeBal_v2.rda')) }
####

####
# functions to organize data

# extract data object from imfr output
getDataObject = function(listObject){
	out = lapply(listObject, function(x){
		 return(x$CompactData$DataSet$Series)
		}) %>% do.call('rbind', .)
	return(out) }

# clean data and organize into df object
cleanDataObject = function(dataObj){
	cleanedData = lapply(1:nrow(dataObj), function(ii){

		# extract data from list element
		raw = dataObj[ii,'Obs'][[1]]

		# some obs have no trading values
		if(is.null(raw)){ return(NULL) }

		# for obs with only one trading value
		# the above subsetting still returns a list
		# this if condition rearranges the data
		if(class(raw)=='list'){
			raw = data.frame(
				t(unlist(raw)), stringsAsFactors=FALSE)
		}

		# clean up variable names and keep only relev cols
		raw = raw[,1:2]
		names(raw) = c("timePd", 'value')
		raw$timePd = char(raw$timePd)
		raw$value = num(raw$value)

		# some timepd vals are monthly
		# or quarterly (e.g., 2014-Q1, 2014-01),
		# create year version of variable
		raw$year = num(gsub('-.*$','',raw$timePd))

		# aggregate year groupings and create
		# final df object
		out = data.frame(
			value=tapply(raw$value, INDEX=raw$year, sum),
			stringsAsFactors=FALSE
		)
		out$year = rownames(out) ; rownames(out) = NULL

		# merge back in id variables
		idData = dataObj[ii,which(names(dataObj)!='Obs')]
		rownames(idData) = rownames(out) = NULL
		out = cbind(idData, out)

		#
		return( out ) })

		return(cleanedData)	 }
####

####
# apply fns to variables pulled form imf
# takes about 3 mins
cl = makeCluster(3)
registerDoParallel(cl)
imfList = foreach(
	x = list(impCifL, expFobL, tradeBal),
	.packages=c('magrittr')
	) %dopar%{
			out = getDataObject(x)
			out = cleanDataObject(out)
			out = do.call('rbind', out)
			return(out)
	}
stopCluster(cl)
####

####
# clean up data and relabel
imfList = lapply(imfList, function(x){

	# save variable name
	varName = char(unique(x[,3]))

	# rearrange and relabel
	x = x[,c(2,4,8,7)]
	names(x) = c('cntry1','cntry2','year','value')
	names(x)[4] = varName

	# subset to only countries in samp
	x = x[x$cntry1 %in% samp$iso,]
	x = x[x$cntry2 %in% samp$iso,]

	# construct id
	x$id = with(x, paste(cntry1, cntry2, year, sep='_'))

	#
	return(x) })

# merge datasets together, first construct
# complete frame
imfData = lapply(imfList,
		function(x){ x$id }) %>%
		unlist() %>% unique() %>% sort()
imfData = data.frame(
	id=imfData, stringsAsFactors=FALSE)
imfData$cntry1 = char(extractFromID(imfData$id, 1))
imfData$cntry2 = char(extractFromID(imfData$id, 2))
imfData$year = num(extractFromID(imfData$id, 3))

# merge in vars from list
for(ii in 1:length(imfList)){
	imfData$tmp = imfList[[ii]][
		match(imfData$id, imfList[[ii]]$id),4]
	names(imfData)[ncol(imfData)] = names(imfList[[ii]])[4] }
rm(imfList)
####

####
# final cleanup and data processing

# relabel
names(imfData)[5:ncol(imfData)] = c('importsCIF', 'exportsFOB', 'tradeBal')

# calculate j-i vals
ijRev = with(imfData, paste(cntry2, cntry1, year, sep='_'))
imfData$exportsCIF = imfData$importsCIF[match(ijRev, imfData$id)]
imfData$importsFOB = imfData$exportsFOB[match(ijRev, imfData$id)]

# convert to dollar units (originally in millions)
imfData[,5:ncol(imfData)] = imfData[,5:ncol(imfData)]*1e6

# add in cnames
imfData$cname1 = samp$cname[match(imfData$cntry1, samp$iso)]
imfData$cname2 = samp$cname[match(imfData$cntry2, samp$iso)]

# convert id var to using cname
imfData$id = with(imfData, paste(cname1, cname2, year, sep='_'))

# reorg
imfData = imfData[,
	c(
		'cname1','cname2','year','id',
		'tradeBal','importsCIF','exportsCIF','importsFOB','exportsFOB') ]

# fill in missing with alternative estimation approach
# (not ideal but these are highly correlated, rho~.98)
imfData$importsCIF[is.na(imfData$importsCIF)] = imfData$importsFOB[is.na(imfData$importsCIF)]
imfData$exportsFOB[is.na(imfData$exportsFOB)] = imfData$exportsCIF[is.na(imfData$exportsFOB)]
####

####
#
save(imfData, file=paste0(dpth, 'imfData.rda'))
####
