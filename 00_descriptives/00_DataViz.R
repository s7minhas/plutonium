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
# viz distraction vars

# subset data
usLevel = read.csv(paste0(pathIn, 'main1_final.csv'))
usLevel = usLevel[order(usLevel$year),]

# reorg data
usLevelD <- usLevel %>%
  select(year, f1_USdeaths_MIDEAST, f2_DEFspend_FORcommits, f3_UE) %>%
  gather(key = "variable", value = "value", -year)

# viz
distViz = ggplot(data = usLevelD, aes(x = year, y = value)) +
  geom_line(aes(color = variable)) +
  geom_point(aes(color = variable)) +
  scale_color_manual(
    values = c("black", "steelblue", "darkred"),
    labels = c("f1", "f2", "f3")) +
  labs(y = "US Distraction Index", x = "Year") +
  theme(legend.position = "top") +
  facet_wrap(~variable)+theme( axis.ticks = element_blank())
# distViz
# ggsave(filename = paste0(pathOut, "distVisFacet.png"), plot = distViz)
####

####
# org data for lat var viz

# subset to relev timeframe
dyadData = dyadData[dyadData$year>=1990,]
####

####
# plaus viz fn
plausViz = function(
  dyadIds, dyadLabs,
  affVar='econScores_tradeDepSend_lfm_v2',
  compVars=c('trade', 'tradeRaw', 'tradeDepSend', 'tradeDepSendRaw'),
  varLabs=c(
    paste0('LFM v', 1:length(affVar)),
    'Trade (z)', 'Trade',
    'Trade Dep. (z)', 'Trade Dep.')){

  # subset to relev pairs and vars
  plausData = dyadData[
    which( dyadData$dyad %in% dyadIds ),
    c('ccode1','ccode2','dyad','year', affVar, compVars)]

  # relebal vars and dyads
  names(plausData)[5:ncol(plausData)] = varLabs
  plausData$dyadAbb = dyadLabs[match(plausData$dyad, dyadIds)]

  # reorg data for plotting
  ggData = melt(plausData[,4:ncol(plausData)], id=c('dyadAbb','year'))

  # convert dyads & vars to factors for ordering
  ggData$dyadAbb = factor(ggData$dyadAbb, levels=dyadLabs)
  ggData$variable = factor(ggData$variable, levels=varLabs)

  # viz
  ggPlaus = ggplot(ggData, aes(x=year, y=value, group=variable, color=variable)) +
    geom_hline(yintercept=0, color='grey', linetype='dashed', size=1) +
    geom_line(size=.8) + geom_point(size=.7) +
    facet_grid(variable~dyadAbb, scales='free_y') +
    labs(color='', shape='') + ylab('') + xlab('') +
    scale_color_brewer(palette='Set1') +
    theme_bw() +
    theme(
      axis.ticks=element_blank(),
      panel.border=element_blank(),
      # legend.text=element_text(family="Source Sans Pro Light"),
      legend.position='top',
      axis.text.x=element_text(
        # family="Source Sans Pro Light",
        angle=45, hjust=1),
      # axis.text.y=element_text(family="Source Sans Pro Light"),
      strip.text.x = element_text(color='white',
                                  # family="Source Sans Pro Semibold"
                                  ),
      strip.text.y = element_text(color='white',
                                  # family="Source Sans Pro Semibold"
                                  ),
      strip.background = element_rect(fill = "#525252", color='#525252')
    )

  #
  return(ggPlaus) }
####

####
# test out results with standardized trade
plausViz(
  dyadIds=c('2_200', '2_365'),
  dyadLabs=c('USA-UK', 'USA-Russia'),
  affVar = c(
    'trade_R2_lfm',
    'trade_R8_lfm',
    'trade_L3_R2_lfm',
    'trade_L5_R2_lfm',
    'trade_L3_R8_lfm',
    'trade_L5_R8_lfm'
     ),
  compVars = c('trade'),
  varLabs = c(
    'k=2, 1yr', 'k=8, 1yr',
    'k=2, 3yr', 'k=2, 5yr',
    'k=8, 3yr', 'k=8, 5yr',
    'trade stdz')
   )

# test out results with standardized trade dependence
plausViz(
  dyadIds=c('2_200', '2_365'),
  dyadLabs=c('USA-UK', 'USA-Russia'),
  affVar = c(
    'tradeDep_R2_lfm',
    'tradeDep_R8_lfm',
    'tradeDep_L3_R2_lfm',
    'tradeDep_L5_R2_lfm',
    'tradeDep_L3_R8_lfm',
    'tradeDep_L5_R8_lfm'
     ),
  compVars = c('tradeDepSend'),
  varLabs = c(
    'k=2, 1yr', 'k=8, 1yr',
    'k=2, 3yr', 'k=2, 5yr',
    'k=8, 3yr', 'k=8, 5yr',
    'trade dep stdz')
   )

# correl check with next year of data
tmp = dyadData
tmp$year = tmp$year - 1
tmp$id = with(tmp, paste(cname1, cname2, year, sep='_'))
dyadData$trade_nextYr = tmp$trade[match(dyadData$id, tmp$id)]
dyadData$tradeDepSend_nextYr = tmp$tradeDepSend[match(dyadData$id, tmp$id)]
vars = c(
  'trade_nextYr',
  'trade',
  'trade_R2_lfm',
  'trade_R8_lfm',
  'trade_L3_R2_lfm',
  'trade_L5_R2_lfm',
  'trade_L3_R8_lfm',
  'trade_L5_R8_lfm' )
cbind(sort(cor(dyadData[,vars], use='pairwise.complete.obs')[-1,-(2:length(vars))], decreasing=TRUE))

vars = c(
  'tradeDepSend_nextYr',
  'tradeDepSend',
  'tradeDep_R2_lfm',
  'tradeDep_R8_lfm',
  'tradeDep_L3_R2_lfm',
  'tradeDep_L5_R2_lfm',
  'tradeDep_L3_R8_lfm',
  'tradeDep_L5_R8_lfm' )
cbind(sort(cor(dyadData[,vars], use='pairwise.complete.obs')[-1,-(2:length(vars))], decreasing=TRUE))

# dyads 1
plausViz(
  dyadIds=c('220_255', '2_200', '710_365'),
  dyadLabs=c('France-Germany', 'USA-UK', 'China-Russia'),
  affVar = c(
    'econScores_tradeDepSend_lfm',
    'econScores_tradeDepSend_lfm_v2' ),
  varLabs = c(
    'LFM Affinity v1', 'LFM Affinity v2',
    'PTA Count', 'Trade Dependence') )

# dyads 2
plausViz(
  dyadIds=c('2_365', '2_645', '2_710'),
  dyadLabs=c('USA-Russia', 'USA-Iraq', 'USA-China'),
  affVar = c(
    'econScores_tradeDepSend_lfm',
    'econScores_tradeDepSend_lfm_v2' ),
  varLabs = c(
    'LFM Affinity v1', 'LFM Affinity v2',
    'PTA Count', 'Trade Dependence') )

# dyads 3
plausViz(
  dyadIds=c('750_770', '630_645', '365_369'),
  dyadLabs=c('India-Pakistan', 'Iran-Iraq', 'Russia-Ukraine'),
  affVar = c(
    'econScores_tradeDepSend_lfm',
    'econScores_tradeDepSend_lfm_v2' ),
  varLabs = c(
    'LFM Affinity v1', 'LFM Affinity v2',
    'PTA Count', 'Trade Dependence') )
####
