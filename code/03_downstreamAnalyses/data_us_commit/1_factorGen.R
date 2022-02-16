####
rm(list=ls())
pth = paste0(here::here(), '/')
source(paste0(pth, 'setup.R'))

#
loadPkg(c(
  'haven',
  'ggplot2', 'dplyr', 'reshape2'
  ))
####

####
dat = read_dta(
  paste0(pathGit, 'code/03_downstreamAnalyses/data_us_commit/main_constraint_1.dta'))
dat = data.frame(dat, stringsAsFactors=F)
####

####
vars = c(
  'def_per_gdp',
  'asia_per',
  'westerneurope_per',
  'easterneurope_per',
  'formersovietunion_per',
  'middleeast_per',
  'africa_per',
  'perforeignmil',
  'f_l_stock_crises',
  'f_l_price_crises',
  'emp_rate',
  'gdp_growth',
  'std_defense_budget',
  'std_totalworld',
  'std_totalformil',
  'std_hostiledeaths' )

#
fData = dat[,vars]
fData = apply(fData, 2, function(x){ (x-mean(x))/sd(x) })

#
faMod = factanal(
  fData, factors=3, lower=0.01, scores='regression')

#
dat = cbind(dat, faMod$scores)
####

####
#
slice = dat[,c(
  'f1_USdeaths_MIDEAST',
  'f2_DEFspend_FORcommits',
  'f3_UE',
  'Factor1', 'Factor2', 'Factor3' )]

round(cor(slice), 2)
####

####
#
faVars = c(
  'f1_USdeaths_MIDEAST',
  'f2_DEFspend_FORcommits', 'f3_UE')
  # ,'Factor1', 'Factor2', 'Factor3' )

#
ggData = melt(dat[,c('year', faVars)], id='year')

#
ggData$varClean = char(ggData$variable)
ggData$varClean[grepl('f1', ggData$variable)] = 'F1 (Active US Conflicts)'
ggData$varClean[grepl('f2', ggData$variable)] = 'F2 (US Defense Spending)'
ggData$varClean[grepl('f3', ggData$variable)] = 'F3 (US Economic Shocks)'

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
  width=8, height=5, device=cairo_pdf )
####
