###Visualization stuff
rm(list=ls())
pth = paste0(here::here(), '/')
source(paste0(pth, 'setup.R'))

#
loadPkg(c(
  'reshape2', 'amen', 'tidyr',
  'ggplot2', 'tidyverse',
  'cshapes', 'countrycode', 'sf' ))

####

####
# load in frame to get all countries
load(paste0(pathIn, 'frame.rda'))
frame = frame[frame$year>=1995,]
cntries = sort(unique(c(frame$cname1, frame$cname2)))
rm(frame)
####

####
# create cntry key
cntryKey = data.frame(
	cname=cntries,
	cown=countrycode(cntries,'country.name', 'cown'),
	cowc=countrycode(cntries, 'country.name', 'cowc') )
cntryKey$cown[
	cntryKey$cname=="KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF"]=731
cntryKey$cown[cntryKey$cname=="SERBIA"]=345
cntryKey$cowc[cntryKey$cname=='SERBIA'] = 'YUG'
####

####
# geo colors for nodes
cmap = wmap = cshp(date=as.Date('2016-1-1'))
wmap$cowc = countrycode(wmap$COWCODE, 'cown', 'cowc')
wmap$cowc[wmap$COWCODE==731]='PRK'
wmap = wmap[which(as.character(wmap$cowc) %in% cntryKey$cowc),]
coords=coordinates(wmap) ; rownames(coords)=wmap$cowc
coords=coords[cntryKey$cowc,]

# Create colors
rlon = pi*coords[,1]/180 ; rlat = pi*coords[,2]/180
slon =  (rlon-min(rlon))/(max(rlon)-min(rlon))
slat =  (rlat-min(rlat))/(max(rlat)-min(rlat))
ccols = rgb( slon^2,slat^2,(1-sqrt(slon*slat))^2)
names(ccols) = cntryKey$cowc ; cntryKey$ccols = ccols
####

####
# Generate legend map
cmap@data$cowc = countrycode(cmap@data$COWCODE, 'cown', 'cowc')
cmap@data$cowc[cmap@data$COWCODE==731]='PRK'
mapCol = ccols[match(cmap$cowc, cntryKey$cowc)]
mapCol[is.na(mapCol)] = 'grey' ; names(mapCol) = cmap@data$cowc

# fortify
cmapDF=fortify(cmap,region='FEATUREID')
names(cmapDF)[6]='FEATUREID'
cmapDF=join(cmapDF, cmap@data)

# viz
ggMap = ggplot() +
	geom_polygon(data=cmapDF, aes(x=long, y=lat,group=group,fill=cowc),color='grey30',size=.05) +
	scale_fill_manual(values=mapCol) +
	coord_equal() + xlab('') + ylab('') +
	theme_bw() +
	theme(
		legend.position = 'none',
		panel.border = element_blank(), panel.grid=element_blank(),
		axis.ticks = element_blank(), axis.line=element_blank(),
		axis.text = element_blank() )
ggsave(ggMap, file=paste0(pathGraphics, 'mapLeg.png'))
save(mapCol, ccols, cntryKey, file=paste0(pathGraphics, 'mapCol.rda'))
####

####
# load back in so we can add to circ
loadPkg(c('grid', 'png'))
mapForCirc = rasterGrob(readPNG(paste0(pathGraphics, 'mapLeg.png')), interpolate=TRUE)
####
