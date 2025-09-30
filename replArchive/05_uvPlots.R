###Visualization stuff
rm(list=ls())
source('setup.R')

#
loadPkg(c(
    'ggplot2', 'ggthemes',
    'grid', 'png', 'Cairo', 'extrafont'))

#
source(paste0(rfuncs, 'ameHelpers.R'))
####

####
load(paste0(rpth, 'modsForApp.rda')) # tradeMods, unMods, icewsMods
mapForCirc = rasterGrob(
  readPNG(
    paste0(gpth, 'mapLeg.png')), interpolate=TRUE)
load(paste0(gpth, 'mapCol.rda'))
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
####

####
# save
ggsave(un00,
  width=6, height=6,
  file=paste0(gpth, 'un00.pdf'),
  device=cairo_pdf)
ggsave(un00,
  width=6, height=6,
  file=paste0(gpth, 'un00.png'),
  dpi=600)

ggsave(un19,
  width=6, height=6,
  file=paste0(gpth, 'un19.pdf'),
  device=cairo_pdf)
ggsave(un19,
  width=6, height=6,
  file=paste0(gpth, 'un19.png'),
  dpi=600)
####
