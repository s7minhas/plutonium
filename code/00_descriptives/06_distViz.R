###Visualization stuff
rm(list=ls())
pth = paste0(here::here(), '/')
source(paste0(pth, 'setup.R'))

#
loadPkg(c(
    'reshape2',
    'ggplot2', 'ggthemes', 'philentropy',
    'grid', 'png', 'Cairo', 'extrafont'))

#
source(paste0(pathFuncs, 'ameHelpers.R'))
####

####
load(paste0(pathOut, 'modsForApp.rda')) # tradeMods, unMods, icewsMods
load(paste0(pathGraphics, 'mapCol.rda'))
####

####
# get dist measurements
dyads = c('USA-CHN', 'USA-RUS', 'USA-UKG')
unDist = getDistData(
  catSelect='UN Voting',
  configSelect = 'agree_k2_srm_lfm',
  paramsToPlot='U',
  distToPlot='cosine',
  dyadVec=dyads)
unDist$cat = 'UN Voting'

tradeDist = getDistData(
  catSelect='Trade',
  configSelect = 'tradeDepSend_k2_srm_lfm',
  paramsToPlot='U',
  distToPlot='cosine',
  dyadVec=dyads)
tradeDist$cat = 'Trade'

#
distData = rbind(unDist, tradeDist)
distData$cat = factor(distData$cat, levels=c('UN Voting', 'Trade'))
####

####
ggDistViz = ggplot(
  distData, aes(x=year, y=value, color=dyad, linetype=dyad) ) +
  geom_point() +
  geom_line() +
  facet_wrap(~cat, scales='free_y', nrow=2) +
  labs(
    x='', y='', color='', linetype=''
  ) +
  scale_color_brewer(palette='Set1') +
  theme_light(base_family="Source Sans Pro") +
  theme(
    legend.position='bottom',
    axis.text.x=element_text(angle=45, hjust=1),
    axis.ticks=element_blank(),
    panel.border=element_blank(),
    strip.text.x = element_text(
      size = 9, color='white',
      family="Source Sans Pro Semibold"),
  	strip.background = element_rect(fill = "#525252", color='#525252')
  )
ggsave(ggDistViz,
  file=paste0(pathPaper, 'distViz.pdf'),
  width=8, height=5,
  device=cairo_pdf )
####
