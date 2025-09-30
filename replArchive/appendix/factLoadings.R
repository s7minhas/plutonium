####
rm(list=ls())
source('setup.R')

#
loadPkg(c(
  'haven', 'tidyr',
  'ggplot2', 'dplyr', 'reshape2'
  ))
####

####
dat = read_dta( paste0(dpth, 'main_constraint_1.dta'))
dat = data.frame(dat, stringsAsFactors=F)

# load factor analysis summaries
loadDat = read.csv( paste0(dpth, 'factLoadings.csv'), stringsAsFactors=FALSE)
####

####
# visualize var loadings
varKey = data.frame(dirty=loadDat$Variable, stringsAsFactors=FALSE)
varKey$clean = c(
	'% Forces in\nMiddle East',
	'Stdz. Deaths\nin Combat',
	'% Forces in\nFormer USSR',
	'Troop Levels',
	'% Forces in Africa',
	'% Forces Abroad',
	'Price Crises',
	'Stock Crises',
	'% Forces in\nE. Europe',
	'Employment Rate',
	'Stdz. Military\nDeaths (Foreign)',
	'GDP Growth',
	'Defense\nSpending/GDP',
	'Stdz. Military\nDeaths (Total)',
	'% Forces in\nW. Europe',
	'% Forces in Asia'
)

#
loadDat$var = varKey$clean[match(loadDat$Variable, varKey$dirty)]

#
ggData = pivot_longer(data=loadDat, cols=Factor1:Factor3)

#
ggData$var = factor(ggData$var,
	levels=c(
	'Defense\nSpending/GDP',
	'Troop Levels',
	'% Forces in Asia',
	'% Forces in\nW. Europe',
	'% Forces in\nE. Europe',
	'% Forces in\nFormer USSR',
	'% Forces in\nMiddle East',
	'% Forces in Africa',
	'% Forces Abroad',
	'Stdz. Military\nDeaths (Total)',
	'Stdz. Military\nDeaths (Foreign)',
	'Stdz. Deaths\nin Combat',
	'Price Crises',
	'Stock Crises',
	'Employment Rate',
	'GDP Growth'
	)
)

#
ggData$name[ggData$name=='Factor1'] = 'F1 (Active US Conflicts)'
ggData$name[ggData$name=='Factor2'] = 'F2 (US Defense Spending)'
ggData$name[ggData$name=='Factor3'] = 'F3'

#
loadViz = ggplot(ggData, aes(x=var, y=value)) +
	geom_bar(stat='identity') +
	facet_wrap(~name, ncol=1, scales='fixed') +
	labs(
		x='', y='Factor Loading'
	) +
  theme_light() +
  theme(
		axis.text.x=element_text(angle=90, hjust=1, size=9),
    legend.position='none',
    axis.ticks=element_blank(),
    panel.border=element_blank(),
    strip.text.x = element_text(
      size = 9, color='white',
      angle=0, hjust=0.05),
    strip.background = element_rect(fill = "#525252", color='#525252')
  )
loadViz
ggsave(loadViz,
  file=paste0(gpth, 'loadViz.pdf'),
  width=8, height=6, device=cairo_pdf )
ggsave(loadViz,
  file=paste0(gpth, 'loadViz.png'),
  width=8, height=6, dpi=600 )
####
