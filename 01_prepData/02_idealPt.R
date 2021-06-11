# get un voting data
####
rm(list=ls())
pth = paste0(here::here(""), '/')
dpth = paste0(pth, 'data/')
load(paste0(dpth, 'samp.rda'))
source(paste0(here::here(), '/setup.R'))
####

####
load(paste0(dpth, 'AgreementScoresAll_Apr2020.Rdata'))
####

####
# load ideal point dataset
idPt = data.frame(dfAgree,stringsAsFactors=FALSE) ; rm(dfAgree)

# post 2000
idPt = idPt[idPt$year>=1975,]

# cntries
cntryKey = data.frame(
	ccode=sort(unique(c(idPt$ccode1, idPt$ccode2))),
	stringsAsFactors = FALSE )
cntryKey$cname = countrycode(cntryKey$ccode, 'cown', 'country.name')
cntryKey$cname[cntryKey$ccode==345] = 'SERBIA'

# north korea fix
cntryKey$cname[
	cntryKey$ccode==731] = "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF"

# add cname to idPt
idPt$cname1 = cntryKey$cname[match(idPt$ccode1,cntryKey$ccode)]
idPt$cname2 = cntryKey$cname[match(idPt$ccode2,cntryKey$ccode)]
idPt = idPt[!is.na(idPt$cname1),]
idPt = idPt[!is.na(idPt$cname2),]

# subset by countries in samp
idPt = idPt[idPt$cname1 %in% samp$cname,]
idPt = idPt[idPt$cname2 %in% samp$cname,]

# create id variable
idPt$id = with(idPt, paste(cname1, cname2, year, sep='_'))

# reorg
idPt = idPt[,c(
	'cname1','cname2','year','id',
	'agree', 'IdealPointDistance'
)]
####

####
# Save to binaries
save(idPt, file=paste0(dpth,'idPt.rda'))
####
