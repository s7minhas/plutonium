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
