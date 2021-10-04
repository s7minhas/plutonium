# get economic agreement data
####
rm(list=ls())
pth = paste0(here::here(), '/')
source(paste0(pth, 'setup.R'))
load(paste0(pathIn, 'samp.rda'))
####

####
# load raw desta
load(paste0(pathIn, 'destaRaw.rda'))
####

####
# subset to relev years
desta = desta[desta$year>=1975,]

# Add cleaned country names
cntry = c(desta$country1, desta$country2) %>%
	unique() %>% data.frame(cntry = .)
cntry$cname = cname(cntry$cntry)

# fix czechia
cntry$cname[cntry$cntry=='Czechia'] = "CZECH REPUBLIC"

# remove sao tome and principe
cntry = cntry[!is.na(cntry$cname),]

# fix congo
cntry$cname[
	cntry$cntry=='Congo - Kinshasa'] = "CONGO, THE DEMOCRATIC REPUBLIC OF"

# add new country names to desta
desta$cname1 = cntry$cname[match(desta$country1, cntry$cntry)]
desta$cname2 = cntry$cname[match(desta$country2, cntry$cntry)]

# subset by countries in samp
desta = desta[desta$cname1 %in% samp$cname,]
desta = desta[desta$cname2 %in% samp$cname,]

# Subset to relev variables in desta
desta = desta[,c('cname1','cname2','year','name')]

# remove nas
desta = na.omit(desta)
####

####
# set up dir dyad desta frame
# Create count of PTAs by year
destaExp = lapply(1:nrow(desta), function(i){
	expand.grid(
		char( desta[i,paste0('cname',1:2)] ),
		char( desta[i,paste0('cname',1:2)] ),
		desta[i,'year']:2020)
	}) %>% do.call('rbind',.) %>% .[.[,1] != .[,2],]

cnts = paste(destaExp[,1],destaExp[,2],destaExp[,3],sep='_') %>%
	table() %>% cbind() %>% data.frame()
cnts$id = rownames(cnts) ; names(cnts)[1] = 'cnt' ; rownames(cnts) = NULL
cnts$idD = unlist(lapply(
	strsplit(cnts$id, '_'), function(i) paste(i[1], i[2], sep='_')))
####

####
# cleanup
desta = cnts
names(desta)[1] = 'ptaCnt'
desta$pta = ifelse(desta$ptaCnt>=1,1,0)

# recreate base id vars
desta$cname1 = char(extractFromID(desta$id, 1))
desta$cname2 = char(extractFromID(desta$id, 2))
desta$year = num(extractFromID(desta$id, 3))

# reorg
desta = desta[,c('cname1','cname2','year','id','ptaCnt','pta')]
####

####
# Save
save(desta, file=paste0(dpth, 'desta.rda'))
####
