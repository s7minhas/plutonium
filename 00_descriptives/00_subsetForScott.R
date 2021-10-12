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
  'treatyBin_R2_lfm',
  'treatyBin_R5_lfm',
  'treatyBin_R8_lfm',
  'treaty_R2_lfm',
  'treaty_R5_lfm',
  'treaty_R8_lfm',
  'icewsScores_gov_lfm',
  'diplomScores_agree_lfm',
  'tradeDep_R2_lfm'
)

forScott = dyadData[,vars]
write.csv(forScott, file=paste0(pathIn, 'forScott.csv'))
save(forScott, file=paste0(pathIn, 'forScott.rda'))
####
