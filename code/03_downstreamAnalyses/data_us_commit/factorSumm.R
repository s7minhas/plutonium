####
rm(list=ls())
pth = paste0(here::here(), '/')
# pth = paste0(pth, 'code/')
source(paste0(pth, 'setup.R'))

#
loadPkg(c(
  'haven', 'tidyr',
  'ggplot2', 'dplyr', 'reshape2'
  ))
####

####
usPth = paste0(pathGit, 'code/03_downstreamAnalyses/data_us_commit/')
dat = read_dta( paste0(usPth, 'main_constraint_1.dta'))
dat = data.frame(dat, stringsAsFactors=F)

# load factor analysis summaries
screeDat = read.csv(paste0(usPth, 'factResults.csv'), stringsAsFactors=FALSE)
####

####
#
screeDat$num = 1:nrow(screeDat)
screeDat = screeDat[,c('num', 'Eigenvalue', 'Cumulative')]
ggData = pivot_longer(screeDat, cols=c('Eigenvalue', 'Cumulative'))

# visualize scree
screeViz = ggplot(ggData, aes(x=num, y=value), group=1) +
	geom_point() + geom_line() +
	labs(x='Number of Factors', y='') +
	facet_wrap(~name, scales='free_y', ncol=1) +
  theme_light(base_family="Source Sans Pro") +
  theme(
    legend.position='none',
    axis.ticks=element_blank(),
    panel.border=element_blank(),
    strip.text.x = element_text(
      size = 9, color='white',
      family="Source Sans Pro Semibold",
      angle=0, hjust=0.05),
    strip.background = element_rect(fill = "#525252", color='#525252')
  )
ggsave(screeViz,
  file=paste0(pathPaper, 'screeViz.pdf'),
  width=8, height=3, device=cairo_pdf )
####

####
#
faVars = c(
  'f1_USdeaths_MIDEAST',
  'f2_DEFspend_FORcommits')

#
ggData = melt(dat[,c('year', faVars)], id='year')

#
ggData$varClean = char(ggData$variable)
ggData$varClean[grepl('f1', ggData$variable)] = 'F1 (Active US Conflicts)'
ggData$varClean[grepl('f2', ggData$variable)] = 'F2 (US Defense Spending)'

#
facViz = ggplot(ggData[ggData$variable %in% faVars[1:3],], aes(x=year, y=value)) +
  geom_line() +
  labs(x='', y='Factor Scores') +
  facet_wrap(~varClean, ncol=1, scales='free_y') +
  theme_light(base_family="Source Sans Pro") +
  theme(
    legend.position='none',
    axis.ticks=element_blank(),
    panel.border=element_blank(),
    strip.text.x = element_text(
      size = 9, color='white',
      family="Source Sans Pro Semibold",
      angle=0, hjust=0.05),
    strip.background = element_rect(fill = "#525252", color='#525252')
  )
ggsave(facViz,
  file=paste0(pathPaper, 'facViz.pdf'),
  width=8, height=4, device=cairo_pdf )
####
