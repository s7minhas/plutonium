####
rm(list=ls())
pth = paste0(here::here(), '/')
dapth = paste0(pth, '03_downstreamAnalyses/')

#
source(paste0(pth, 'setup.R'))

#
library(tidyverse)
library(ggplot2)
library(extrafont)
library(Cairo)
library(RColorBrewer)
suppressMessages(loadfonts(device='win'))
####

####
# dyad pref data + icews
load(paste0(dapth, 'dyadData.rda'))
names(dyadData) = gsub('_','',names(dyadData))
####

####
# during data processing matlConf and verbConf
# were flipped via multiplying by -1
# here we change that back
toReFlip = paste0(c('matlConf','verbConf'),c('Gov'))
dyadData[,toReFlip] = dyadData[,toReFlip]*-1
####

####
# aggregate dyad-yearly data to yearly
cntryKey = data.frame(
  long = c(
    'UNITED STATES',
    'RUSSIAN FEDERATION',
    'CHINA',
    'PAKISTAN',
    'IRAN, ISLAMIC REPUBLIC OF',
    'SAUDI ARABIA',
    'JAPAN',
    'KOREA, REPUBLIC OF',
    'ESTONIA',
    'FRANCE',
    'GERMANY',
    'UNITED KINGDOM' ),
  short = c(
    'usa', 'russia', 'china', 'paki', 'iran', 'saudi', 'japan',
    'skorea', 'estonia', 'france', 'germany', 'uk'
  ), stringsAsFactors=FALSE )

#
varsToAgg = names(dyadData)[c(
	14:17, # icews vars
	32:ncol(dyadData) # net measures
)]

# for every country in cntriesInt
# calc the avg distance of all other
# cntries by year
# fn to implement
createSysVars = function(data, cntry, cntryShort, vars){
  require(dplyr)
  sysLevel = data %>%
    group_by(year) %>%
    filter(cname1==cntry) %>%
    summarise_at( vars, mean, na.rm=TRUE )
  names(sysLevel)[2:ncol(sysLevel)] = paste0(
    names(sysLevel)[2:ncol(sysLevel)], '_', cntryShort)
  return(sysLevel) }

# lapply through
sysData = lapply(1:nrow(cntryKey), function(ii){
  cntryData = createSysVars(
    dyadData, cntryKey$long[ii], cntryKey$short[ii], varsToAgg)
  return(cntryData) })
####

####
# merge with scott's data
# load scott's stuff
usLevel = read.csv(paste0(dapth, '/main1_final.csv'))
usLevel = usLevel[order(usLevel$year),]

# merge in with scott's stuff
for(ii in 1:length(sysData)){
  usLevel = merge(usLevel, sysData[[ii]], by='year') }

# save merged file
write.csv(usLevel, file=paste0(dapth, '/aggData.csv'))
####

####
# run correlation analysis
corVars = names(usLevel)[c(33:ncol(usLevel))]

#
corrMatrix = cor(usLevel[,corVars], use='pairwise.complete.obs')

#
corrMatrix = corrMatrix[-(1:3),1:3]

# add some cols
corrDF = data.frame(corrMatrix, stringsAsFactors=FALSE)
corrDF$fullVar = rownames(corrDF)
corrDF$var = unlist(lapply(strsplit(corrDF$fullVar,'_'), function(x){x[1]}))
corrDF$cntry = unlist(lapply(strsplit(corrDF$fullVar,'_'), function(x){x[2]}))

# reorg
rownames(corrDF) = NULL
corrDF = corrDF[,c(4:6,1:3)]
####

####
# drop countries irrelevant to brief
# corrDF = corrDF[!(corrDF$cntry %in% c('germany','uk','france')),]
####

####
# transform data so that all the pca distraction vars
# are going down a column, will enable easier plotting
corrDF = pivot_longer(
  data=corrDF,
  cols=f1_USdeaths_MIDEAST:f3_UE,
  names_to='pcaVar',
  values_to='pcaVal'
)
####

####
# drop certain configs

# drop us economy pca
corrDF = corrDF[corrDF$pcaVar!='f3_UE',]
####

####
# clean up vars
cntryKey = data.frame( dirty=unique(corrDF$cntry), stringsAsFactors=F )
cntryKey$clean = c(
  'United States', 'Russia', 'China', 'Pakistan', 'Iran',
  'Saudi Arabia', 'Japan', 'South Korea', 'Estonia',
  'France', 'Germany', 'United Kingdom' )
cntryKey = cntryKey[c(1,10:12,7,8,6,9,4,3,2,5),]

varKey = data.frame( dirty=unique(corrDF$var), stringsAsFactors=F )
varKey$clean = c(
  'Material Conflict',
  'Material Cooperation',
  'Verbal Cooperation',
  'Verbal Conflict',
  rep(NA, nrow(varKey)-4) )
varKey$clean[grepl('diplomScores',varKey$dirty)] = 'Avg. Diplomacy Similarity'
varKey$clean[grepl('econScores',varKey$dirty)] = 'Avg. Economic Similarity'
varKey$clean[grepl('icewsScores',varKey$dirty)] = 'Avg. ICEWS Similarity'
varKeyOrder = unique(varKey$clean)
varKeyOrder = varKeyOrder[c(1,2,4,3,6,5,7)]

pcaKey = data.frame( dirty=unique(corrDF$pcaVar), stringsAsFactors=F )
pcaKey$clean = c(
  'US Troop\nDeaths Factor',
  'US Defense\nSpending Factor'
  # ,'US Economy'
)

corrDF$cntryClean = cntryKey$clean[match(corrDF$cntry, cntryKey$dirty)]
corrDF$cntryClean = factor(corrDF$cntryClean, levels=cntryKey$clean)

corrDF$varClean = varKey$clean[match(corrDF$var, varKey$dirty)]
corrDF$varClean = factor(corrDF$varClean, levels=rev(varKeyOrder))

corrDF$pcaVarClean = pcaKey$clean[match(corrDF$pcaVar, pcaKey$dirty)]
corrDF$pcaVarClean = factor(corrDF$pcaVarClean, levels=pcaKey$clean)
####

####
# separate corr stuff
vars = unique(corrDF$var)
icewVars = paste0(pasteVec(c('matl','verb'),c('Conf','Coop')),'Gov')
mlVars = vars[grepl('ML',vars)]
lfmVars = vars[grepl('lfm',vars)]
mltrVars = setdiff( vars, c(icewVars, mlVars, lfmVars) )

# divide vars into groups
varGroups = list(
	'icew'= icewVars, 'mltr'=mltrVars,
  'ml'=mlVars, 'lfm'=lfmVars )

# create sep dfs by group
ggDataL = lapply(1:length(varGroups), function(ii){
	slice = corrDF[corrDF$var %in% varGroups[[ii]],]
	slice$modType = names(varGroups)[ii]
	return(slice) })

# create viz
colVec = c('#bdbdbd', '#636363')
vizL = lapply(ggDataL, function(ggData){

	ggViz=ggplot(ggData, aes(x=varClean, y=pcaVal, fill=factor(pcaVarClean))) +
	  geom_bar(stat='identity', position='dodge2', color='black', size=.05) +
	  facet_wrap(~cntryClean, nrow=4, scales='fixed') +
	  coord_flip() +
	  labs(x='',y='Correlation Coefficient', fill='') +
	  theme_bw() +
	  scale_fill_manual(values=colVec) +
	  theme(
	    panel.border=element_blank(),
	    axis.ticks=element_blank(),
	    axis.title.x=element_text(size=8, family='Source Sans Pro Semibold'),
	    axis.text.x=element_text(size=6, family='Source Sans Pro'),
	    axis.text.y=element_text(size=8, family='Source Sans Pro'),
	    legend.text=element_text(size=8, family='Source Sans Pro'),
			strip.text.x = element_text(size = 8, color='white',
	  		family="Source Sans Pro Semibold",
	  		angle=0, hjust=.05),
	  	strip.background = element_rect(fill = "#525252", color='#525252'),
	    legend.position='top'
	  )

	mType = unique(ggData$modType)
	ggsave(ggViz, file=paste0(dapth, 'corr_',mType,'.png'),
	  width=8, height=6, type='cairo-png')
})
####
