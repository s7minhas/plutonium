###Visualization stuff
rm(list=ls())
source('setup.R')

#
loadPkg(c(
    'reshape2',
    'ggplot2', 'ggthemes', 'philentropy',
    'grid', 'png', 'Cairo', 'extrafont'))

#
source(paste0(rfuncs, 'ameHelpers.R'))
####

####
load(paste0(rpth, 'modsForApp.rda')) # tradeMods, unMods, icewsMods
load(paste0(gpth, 'mapCol.rda'))
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
unDist$cat = 'Diplomatic Alignment'

#
# distData = rbind(unDist, tradeDist)
# distData$cat = factor(distData$cat,
# 	levels=c('Diplomatic Alignment', 'Trade Alignment'))
distData = unDist
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
      family="Source Sans Pro Semibold",
      angle=0, hjust=0.05),
  	strip.background = element_rect(fill = "#525252", color='#525252')
  )
ggsave(ggDistViz,
  file=paste0(gpth, 'distViz.pdf'),
  width=8, height=4,
  device=cairo_pdf )
ggsave(ggDistViz,
  file=paste0(gpth, 'distViz.png'),
  width=8, height=4,
  dpi=600 )
####
