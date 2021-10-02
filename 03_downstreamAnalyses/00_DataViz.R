###Visualization stuff

rm(list=ls())
here::set_here()
pth = paste0(here::here(), '/')
dapth = paste0(pth, '03_downstreamAnalyses/')

#
source(paste0(pth, 'setup.R'))

#
loadPkg('cshapes')
####

####
load(paste0(pathIn, "dyadData.rda"))
####


usLevel = read.csv(paste0(pathIn, 'main1_final.csv'))
usLevel = usLevel[order(usLevel$year),]

library(ggplot2)


ggplot(data = usLevel, aes(x = year, y = f1_USdeaths_MIDEAST)) + geom_line() + geom_point()
ggplot(data = usLevel, aes(x = year, y = f2_DEFspend_FORcommits)) + geom_line() + geom_point()
ggplot(data = usLevel, aes(x = year, y = f3_UE)) + geom_line() + geom_point()

par(mfrow = c(3,1))
f1 = ggplot(data = usLevel) + geom_line(aes(x = year, y = f1_USdeaths_MIDEAST), color = "black") + geom_point(aes(x = year, y = f1_USdeaths_MIDEAST), color = "black")  +  geom_line(aes(x = year, y = f2_DEFspend_FORcommits), color = "blue") + geom_point(aes(x = year, y = f2_DEFspend_FORcommits), color = "blue") + geom_line(aes(x = year, y = f3_UE), color = "red") + geom_point(aes(x = year, y = f3_UE), color = "red") + labs(y = "US Distraction Index", x = "Year")
f2 = ggplot(data = usLevel, aes(x = year, y = f2_DEFspend_FORcommits)) + geom_line() + geom_point()
f3 = ggplot(data = usLevel, aes(x = year, y = f3_UE)) + geom_line() + geom_point()

loadPkg('tidyverse')

usLevelD <- usLevel %>%
  select(year, f1_USdeaths_MIDEAST, f2_DEFspend_FORcommits, f3_UE) %>%
  gather(key = "variable", value = "value", -year)

f1 = ggplot(data = usLevelD, aes(x = year, y = value)) + geom_line(aes(color = variable)) + geom_point(aes(color = variable)) + scale_color_manual(values = c("black", "steelblue", "darkred"), labels = c("f1", "f2", "f3")) + labs(y = "US Distraction Index", x = "Year") + theme(legend.position = "top") +theme( axis.ticks = element_blank())
f2 = ggplot(data = usLevelD, aes(x = year, y = value)) + geom_line(aes(color = variable)) + geom_point(aes(color = variable)) + scale_color_manual(values = c("black", "steelblue", "darkred"), labels = c("f1", "f2", "f3")) + labs(y = "US Distraction Index", x = "Year") + theme(legend.position = "top") + facet_wrap(~variable)+theme( axis.ticks = element_blank())

ggsave(filename = paste0(pathOut, "distVis.png"), plot = f1)
ggsave(filename = paste0(pathOut, "distVisFacet.png"), plot = f2)

library(countrycode)
dyadData$ccode1 = countrycode(dyadData$cname1, "country.name", "cown")
dyadData$ccode2 = countrycode(dyadData$cname2, "country.name", "cown")
dyadData$dyad = paste(dyadData$ccode1, dyadData$ccode2, sep ="_")

plausPlot = function(dyadIds, dyadLabs, pW=8, pH=5, fName){
  fig1Plaus = dyadData[
    which( dyadData$dyad %in% dyadIds ),
    c('ccode1','ccode2','dyad','year','econScores_tradeDepSend_lfm')] 
  names(fig1Plaus)[5] = c('LFM Affinity')
  fig1Plaus$dyadAbb = NA
  for(i in 1:length(dyadIds)){ fig1Plaus$dyadAbb[fig1Plaus$dyad==dyadIds[i]]=dyadLabs[i] }
  ggFig1 = reshape2::melt(fig1Plaus[,4:6], id=c('dyadAbb','year'))
  ggFig1$dyadAbb = factor(ggFig1$dyadAbb, levels=dyadLabs)
  ggFig1$variable = factor(ggFig1$variable, levels=names(fig1Plaus)[5])
  

  ggPlaus = ggplot(ggFig1, aes(x=year, y=value, group=variable, color=variable)) +
    geom_hline(yintercept=0, color='grey', linetype='dashed', size=1) +	
    geom_line(size=.8) + geom_point(aes(shape=variable), size=1.5) +
    scale_color_brewer(palette='Set1') + 
    facet_grid(variable~dyadAbb, scales='free') +
    labs(color='', shape='') + ylab('') + xlab('') +
    theme(
      axis.ticks=element_blank(),
      # panel.border=element_blank(),
      legend.text=element_text(family="Source Sans Pro Light"),
      legend.position='top',
      axis.text.x=element_text(family="Source Sans Pro Light", angle=45, hjust=1),
      axis.text.y=element_text(family="Source Sans Pro Light"),
      strip.text.x = element_text(color='white',
                                  family="Source Sans Pro Semibold"),
      strip.text.y = element_text(color='white',
                                  family="Source Sans Pro Semibold"),			
      strip.background = element_rect(fill = "#525252", color='#525252')				
    )
  ggsave(ggPlaus, file=fName, width=pW, height=pH, device=cairo_pdf)
}

plausPlot(
  dyadIds=c('220_255', '2_200', '710_365'),
  dyadLabs=c('France-Germany', 'USA-UK', 'China-Russia'),
  fName=paste0(pathGraphics, 'plausPlot_1_border.pdf')
)

plausPlot(
  dyadIds=c('2_365', '2_645', '2_710'),
  dyadLabs=c('USA-Russia', 'USA-Iraq', 'USA-China'),
  fName=paste0(pathGraphics, 'plausPlot_2_border.pdf')
)


plausPlot(
  dyadIds=c('750_770', '630_645', '365_369'),
  dyadLabs=c('India-Pakistan', 'Iran-Iraq', 'Russia-Ukraine'),
  fName=paste0(pathGraphics, 'plausPlot_3_border.pdf')
)
