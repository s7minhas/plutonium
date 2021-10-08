###Visualization stuff
rm(list=ls())
pth = paste0(here::here(), '/')
source(paste0(pth, 'setup.R'))

#
loadPkg(c(
  'scales', 'sf',
  'reshape2', 'igraph',
  'ggplot2', 'tidyverse',
  'cshapes', 'countrycode' ))
####

####
load(paste0(pathIn, "dyadData.rda"))
####

####
# choose colors for countries based on geo position
cntries=unique(c(dyadData$cname1, dyadData$cname2))
wmap = cshp(date=as.Date('2012-1-1'))
wmap$cname = cname(wmap$country_name)
wmap$cname[wmap$country_name=='Belarus (Byelorussia)'] = cname('Belarus')
wmap$cname[wmap$country_name=='Rumania'] = cname('Romania')
wmap$cname[wmap$country_name=="Korea, People's Republic of"] = cname('North Korea')
cmap = wmap
wmap = wmap[which(as.character(wmap$cname) %in% cntries),]
coords=data.frame(wmap)[,c('caplong','caplat')]
coords = data.matrix(coords)
rownames(coords)=wmap$cname
coords=coords[cntries,]

# Create colors
rlon = pi*coords[,1]/180
rlat = pi*coords[,2]/180

slon =  (rlon-min(rlon))/(max(rlon)-min(rlon))
slat =  (rlat-min(rlat))/(max(rlat)-min(rlat))
ccols = rgb( slon^2,slat^2,(1-sqrt(slon*slat))^2)
names(ccols) = cntries

# Generate legend map
# if(genCntryMap){
	mapCol = ccols[match(cmap$cname, cntries)]
	mapCol[is.na(mapCol)] = 'grey'
	# fname=paste0(outPath, 'map.eps')
	# postscript(file=fname, width=8, height=4, horizontal=FALSE, onefile = FALSE, paper = "special")
	# plot(cmap, col=mapCol, max.plot=1, main='')
  # plot(cmap)
	# dev.off()
# }
####

####
# pick a var
var = 'tradeRaw'
degreeCalc = 'total' # send, rec, total
####

####
# org data for network
netData = dyadData[
  dyadData$year>=1990,
  c('cname1','cname2','year',var)
  ]

# so we need to split this data up by year
# and for every year we will produce one net viz
# we're going to store the data as a list
# of adjacency matrices
adjMatList = lapply(
  unique(netData$year), function(yr){

  # subset data by time
  slice = netData[netData$year==yr,]

  # first step in constructing an adjmat
  # is having your vector of actors
  actors = sort(unique(c(slice$cname1, slice$cname2)))
  nActs = length(actors)

  # lets set up the bones of our adjacency matrix
  adjMat = matrix(
    NA, nrow=nActs, ncol=nActs,
    dimnames=list(actors, actors) )

  # now lets fill in our adjmat
  for(ii in 1:nrow(slice)){

    # identify relev comps from slice
    a1 = slice$cname1[ii]
    a2 = slice$cname2[ii]
    val = slice[ii,var]

    # fill in matrix
    adjMat[a1,a2] = val }

  #
  return(adjMat) } )

# so now lets create an igraph version
# igList = lapply(adjMatList, function(adjMat){

adjMat = adjMatList[[1]]

  ###
  adjMat = log(adjMat + 1)
  ###

  ###
  # org actor names
  actors = rownames(adjMat)
  abbs = countrycode(actors, 'country.name', 'iso3c')
  rownames(adjMat) = colnames(adjMat) = abbs
  ###

  ###
  # convert to igraph object
  g = graph_from_adjacency_matrix(
    adjMat,
    mode = c("directed"),
    weighted = TRUE,
    diag = FALSE )

  # remove isolates
  isoCases = which(degree(g)==0)
  g = delete.vertices(g, isoCases)
  ###

  ###
  # add nodal attributes

  # degree calculation
  if(degreeCalc == 'total') {
    actDegree = rowSums(adjMat, na.rm=TRUE) + colSums(adjMat, na.rm=TRUE) }
  if(degreeCalc=='send'){
  actDegree = rowSums(adjMat, na.rm=TRUE) }
  if(degreeCalc=='rec'){
    actDegree = colSums(adjMat, na.rm=TRUE) }

  g$labSize = rescale(actDegree, c(.4, .7))
	g$vSize = rescale(actDegree, c(10, 16))
	g$vShape = rep('circle', length(V(g)))
  g$vColor = ccols[match(actors, names(ccols))]
ccols[order(names(ccols))]

plot(
  g,
  layout=layout_with_fr(g),
  # vertex.label.color=''
  vertex.label.cex = g$labSize,
  vertex.size = g$vSize,
  vertex.color=g$vColor,
  vertex.label.color='black',
  edge.width=rescale(E(g)$weight, .1, 2),
  edge.arrow.size=0,
  edge.curved=TRUE
)

  })
####
