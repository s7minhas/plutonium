###Visualization stuff
rm(list=ls())
pth = paste0(here::here(), '/')
source(paste0(pth, 'setup.R'))

#
loadPkg(c(
  'reshape2',
  'ggplot2', 'tidyverse',
  'cshapes', 'countrycode' ))
####

####
load(paste0(pathIn, "dyadData.rda"))
####

####
# subset to relev timeframe
dyadData = dyadData[dyadData$year>=1990,]
####

####
# create slice of data for scott
vars = c(
  'cname1',
  'cname2',
  'ccode1',
  'ccode2',
  'dyad',
  'year',
  'id',
  'trade',
  'tradeRaw',
  'allyTotalRaw',
  'ptaCntRaw',
  'treatyCoopRaw',
  'agree',
  'matlCoopGov',
  'verbCoopGov',
  'matlConfGov',
  'verbConfGov',
  'treatyBin_R2_lfm',
  'treatyBin_R5_lfm',
  'treatyBin_R8_lfm',
  'treaty_R2_lfm',
  'treaty_R5_lfm',
  'treaty_R8_lfm',
  'diplomScores_agree_lfm',
  'tradeDep_R2_lfm',
	'icewsScores_R2_lfm', 'icewsScores_R5_lfm', 'icewsScores_R8_lfm',
	'icewsCoopScores_R2_lfm', 'icewsCoopScores_R5_lfm', 'icewsCoopScores_R8_lfm',
	'matlCoopScores_R2_lfm', 'matlCoopScores_R5_lfm', 'matlCoopScores_R8_lfm',
	'verbCoopScores_R2_lfm', 'verbCoopScores_R5_lfm', 'verbCoopScores_R8_lfm'
)

forScott = dyadData[,vars]
write.csv(forScott, file=paste0(pathIn, 'forScott.csv'))
save(forScott, file=paste0(pathIn, 'forScott.rda'))
####
