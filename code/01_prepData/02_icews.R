####
rm(list=ls())
pth = paste0(here::here(""), '/')
dpth = paste0(pth, 'data/')
load(paste0(dpth, 'samp.rda'))
source(paste0(here::here(), '/setup.R'))

# load additional pkgs
loadPkg(c('readr', 'tidyr'))
####

####
# pull in icews data
# get file paths for icews data
icewsFiles = list.files(paste0(dpth, 'icewsDataverse/')) %>%
	# .[grepl(paste(paste0('events.',2000:2018),collapse='|'), .)] %>%
	.[grepl(paste(paste0('events.',1995:2020),collapse='|'), .)] %>%
	paste0(dpth, 'icewsDataverse/', .)

# read in codebook for quad vars
### could also have done substr match based on first two digits
quadCodebook = read.csv(
	paste0(dpth,
		'icewsDataverse/icewsQuadCodebook.csv') )

# get list of cntries in icews
if(!file.exists(paste0(dpth, 'icewsDataverse/icewsCntries.rda'))){
	icewsCntries = lapply(icewsFiles, function(iFile){
		iData = suppressMessages(read_tsv(iFile, progress=FALSE)) %>%
			data.frame(.,stringsAsFactors = FALSE)
		cntries = unique(c(iData$Source.Country, iData$Target.Country))
		return( cntries[!is.na(cntries)] ) }) %>% unlist() %>% unique() %>% sort()
	icewsCntries = data.frame(iName = icewsCntries, stringsAsFactors = FALSE)
	icewsCntries$cname = countrycode(icewsCntries$iName, 'country.name', 'country.name')

	# remove curacao and us virgin islands
	icewsCntries = icewsCntries[!is.na(icewsCntries$cname),]
	save(icewsCntries,
		file=paste0(dpth, 'icewsDataverse/icewsCntries.rda'))
} else {
	load(paste0(dpth, 'icewsDataverse/icewsCntries.rda')) }

# run through icews files and subset by relevant events,
# specifically only interstate gov-gov interactions
# also subset countries to those included in cntryKey
# last get aggregated counts of quad variables
icewsQuad = lapply(icewsFiles, function(iFile){
	iData = suppressWarnings(
		suppressMessages(
			read_tsv(iFile, progress=FALSE)
		) ) %>%
		data.frame(.,stringsAsFactors = FALSE)

	# remove intrastate interactions
	iData = iData[which(iData$Source.Country != iData$Target.Country),]

	# keep only gov - gov interactions
	iData = iData[!is.na(iData$Source.Sectors),]
	iData = iData[!is.na(iData$Target.Sectors),]
	iData = iData[grepl('Government', iData$Source.Sectors),]
	iData = iData[grepl('Government', iData$Target.Sectors),]

	# add quad var classification
	iData$quadVar = quadCodebook$quadVar[
		match(iData$Event.Text, quadCodebook$name)
		]
	iData = iData[!is.na(iData$quadVar),]

	# country renaming
	iData$cname1 = countrycode(iData$Source.Country,
		'country.name', 'country.name')
	iData$cname2 = countrycode(iData$Target.Country,
		'country.name', 'country.name')

	# keep only countries that are also in samp
	iData = iData[which(iData$cname1 %in% samp$cname),]
	iData = iData[which(iData$cname2 %in% samp$cname),]

	# get right date format and aggregate
	iData$year = strsplit(char(iData$Event.Date), '-') %>%
		lapply(.,function(x){x[1]}) %>% unlist() %>% num()
	iData$id = with(iData, paste(cname1, cname2, year, sep='_'))

	# aggregate
	iAgg = iData %>%
		group_by(cname1, cname2, year, id) %>%
		count(quadVar) %>%
		spread(quadVar, n, fill=0) %>%
		data.frame(.,stringsAsFactors = FALSE)

	# return
	return(iAgg) }) %>% do.call('rbind', .)
####

####
# save if using raw icews
save(icewsQuad,
	file=paste0(dpth, 'icewsQuad.rda'))
####

####
# rename vars and cleanup
icewsQuadGov = icewsQuad
names(icewsQuadGov)[5:ncol(icewsQuadGov)] = paste0(
	names(icewsQuadGov)[5:ncol(icewsQuadGov)],
	'Gov' )

# save if using icews gov-gov version
save(icewsQuadGov,
	file=paste0(dpth, 'icewsQuadGov.rda'))
####
