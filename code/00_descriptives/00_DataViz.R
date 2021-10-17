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
  affVar=c('treatyBin_R2_lfm','icewsScores_gov_lfm','diplomScores_agree_lfm'), color=TRUE,
  compVars=NULL,
  varLabs=c("Econ and Treaty LFM", "ICEWS LFM", "Diplom LFM")){

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
  if(color){
    ggPlaus = ggplot(ggData,
        aes(x=year, y=value, group=variable, color=variable)) +
      scale_color_brewer(palette='Set1')
  } else {
    ggPlaus = ggplot(
      ggData, aes(x=year, y=value, group=variable)) }
  ggPlaus = ggPlaus +
    geom_hline(yintercept=0, color='grey', linetype='dashed', size=1) +
    geom_line(size=.8) + geom_point(size=.7) +
    facet_grid(variable~dyadAbb, scales='free_y') +
    labs(color='', shape='') + ylab('') + xlab('') +
    theme_bw() +
    theme(
      axis.ticks=element_blank(),
      # panel.border=element_blank(),
      legend.text=element_text(family="Source Sans Pro Light"),
      legend.position='top',
      axis.text.x=element_text(
        family="Source Sans Pro Light",
        angle=45, hjust=1),
      axis.text.y=element_text(family="Source Sans Pro Light"),
      strip.text.x = element_text(color='white',
                                  family="Source Sans Pro Semibold"
                                  ),
      strip.text.y = element_text(color='white',
                                  family="Source Sans Pro Semibold"
                                  ),
      strip.background = element_rect(fill = "#525252", color='#525252')
    )

  #
  return(ggPlaus) }
####

####
# test out results for a number of dyads, to look at a new dyad, change the dyadIds (and the labels)
USStuff = plausViz(
  dyadIds=c('2_200', '2_365', '2_710'),
  dyadLabs=c('USA-UK', 'USA-Russia', 'USA-China'),
  affVar = c('treatyBin_R2_lfm','icewsScores_gov_lfm'),
  compVars = 'diplomScores_agree_lfm',
  varLabs = c("Econ + Treaties", "ICEWS", "Diplomacy") )
ggsave(USStuff,
  width=8, height=6,
  file=paste0(pathGraphics, 'boozCompareecon.pdf'), device=cairo_pdf)

# test out results with standardized trade dependence
Friends = plausViz(
  dyadIds=c('2_666', '2_732', '220_255'),
  dyadLabs=c('USA-Israel', 'US-ROK', 'France-Germany'),
  affVar = c('treatyBin_R2_lfm','icewsScores_gov_lfm'),
  compVars = 'diplomScores_agree_lfm',
  varLabs = c("Econ + Treaties", "ICEWS", "Diplomacy") )
ggsave(Friends,
  width=8, height=6,
  file=paste0(pathGraphics, 'boozFriendsEcon.pdf'), device=cairo_pdf)
####

Rivals = plausViz(
  dyadIds=c('750_770', '365_369', '710_740'),
  dyadLabs=c('India-Pakistan', 'Russia-Ukraine', 'China-Japan'),
  affVar = c('treatyBin_R2_lfm','icewsScores_gov_lfm'),
  compVars = 'diplomScores_agree_lfm',
  varLabs = c("Econ + Treaties", "ICEWS", "Diplomacy") )
ggsave(Rivals,
       width=8, height=6,
       file=paste0(pathGraphics, 'boozRivalsEcon.pdf'), device=cairo_pdf)



Random = plausViz(
  dyadIds=c('2_704', '710_349', '640_135'),
  dyadLabs=c('US-Uzbekistan', 'China-Slovenia', 'Turkey-Peru'),
  affVar = c('treatyBin_R2_lfm','icewsScores_gov_lfm'),
  compVars = 'diplomScores_agree_lfm',
  varLabs = c("Econ + Treaties", "ICEWS", "Diplomacy") )
ggsave(Random,
       width=8, height=6,
       file=paste0(pathGraphics, 'boozRandomEcon.pdf'), device=cairo_pdf)

SecretPals = plausViz(
  dyadIds=c('666_670', '666_630', '630_670'),
  dyadLabs=c('Israel-KSA', 'Israel-Iran', 'Iran-KSA'),
  affVar = c('treatyBin_R2_lfm','icewsScores_gov_lfm'),
  compVars = 'diplomScores_agree_lfm',
  varLabs = c("Econ + Treaties", "ICEWS", "Diplomacy") )
ggsave(SecretPals,
       width=8, height=6,
       file=paste0(pathGraphics, 'boozMideastEcon.pdf'), device=cairo_pdf)


####
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


##Variable density by year

p1 <- ggplot(data=dyadData, aes(x=treatyBin_R2_lfm, group=year, fill=year)) +
  geom_density(adjust=1.5, alpha = .2)

ggsave(paste0(pathGraphics, "bindensity.pdf"), p1)


p1 <- ggplot(data=dyadData, aes(x=treaty_R8_lfm, group=year, fill=year)) +
  geom_density(adjust=1.5, alpha = .2)

ggsave(paste0(pathGraphics, "raw8Density.pdf"), p1)


toUse = dyadData[,c(1:4,50:53, 82, 47)]
save(toUse, file = paste0(pathDrop,"forScott.rda"))
