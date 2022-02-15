###Visualization stuff
rm(list=ls())
pth = paste0(here::here(), '/')
source(paste0(pth, 'setup.R'))

#
loadPkg(c(
    'ggplot2', 'ggthemes',
    'grid', 'png', 'Cairo', 'extrafont'))

#
source(paste0(pathFuncs, 'ameHelpers.R'))
####

####
load(paste0(pathOut, 'modsForApp.rda')) # tradeMods, unMods, icewsMods
mapForCirc = rasterGrob(
  readPNG(
    paste0(pathGraphics, 'mapLeg.png')), interpolate=TRUE)
load(paste0(pathGraphics, 'mapCol.rda'))
####

####
un00 = uvViz(
  catSelect='UN Voting', configSelect='agree_k2_srm_lfm',
  paramsToPlot='U', timeSelect=2000,
  cntryVec=c(
    'USA', 'UKG', 'CAN', 'GMY', 'JPN',
    'AUL', 'AFG', 'RUS', 'SYR', 'CHN' ) )

un19 = uvViz(
  catSelect='UN Voting', configSelect='agree_k2_srm_lfm',
  paramsToPlot='U', timeSelect=2019,
  cntryVec=c(
    'CAN', 'ISR', 'USA', 'AUL', 'UKG',
    'JPN', 'GMY', 'RUS', 'CHN', 'SYR', 'AFG' ) )

trade00 = uvViz(
  catSelect='Trade', configSelect='tradeDepSend_k2_srm_lfm',
  paramsToPlot='U', timeSelect=2000,
  cntryVec=c(
    'AUL', 'CHN', 'JPN', 'UKG', 'USA',
    'ISR', 'CAN', 'GMY', 'SYR', 'AFG' ) )

trade20 = uvViz(
  catSelect='Trade', configSelect='tradeDepSend_k2_srm_lfm',
  paramsToPlot='U', timeSelect=2020,
  cntryVec=c(
    'SYR', 'RUS', 'AFG', 'AUL', 'CHN',
    'JPN', 'USA', 'CAN', 'ISR', 'UKG', 'GMY' ) )
####

####
# save
ggsave(un00,
  width=6, height=6,
  file=paste0(pathPaper, 'un00.pdf'),
  device=cairo_pdf)

ggsave(un19,
  width=6, height=6,
  file=paste0(pathPaper, 'un19.pdf'),
  device=cairo_pdf)

ggsave(trade00,
  width=6, height=6,
  file=paste0(pathPaper, 'trade00.pdf'),
  device=cairo_pdf)

ggsave(trade20,
  width=6, height=6,
  file=paste0(pathPaper, 'trade20.pdf'),
  device=cairo_pdf)
####
