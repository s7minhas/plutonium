####
pth = paste0(here::here(), '/')
source(paste0(pth, 'setup.R'))

#
pkgs = c(
  'ggplot2', 'patchwork',
  'countrycode',
  'sp', 'rworldmap',
  'viridis', 'viridisLite')
loadPkg(pkgs)
####

####
makeMapRegion = function(dat, var, legWidth=1, grad2=TRUE){

  # desig colVar
  dat$colVar = dat[,var]

  # set up gg frame
  gg = ggplot(dat, aes(x=long, y=lat, group=group)) +
    geom_polygon(
      aes(group=group, fill=colVar),
      color='grey20', size=.1 )

  if(grad2){
    gg = gg + scale_fill_gradient2() }

  if(!grad2){
    gg = gg + scale_fill_viridis(discrete=FALSE) }

  gg = gg +
    coord_equal(
      ylim=c(-6e+06,.9e+07),
      xlim=c(-1.3e+07, 1.8e+07) ) +
    theme_void() +
    theme(
      legend.position='top',
      legend.key.width=unit(legWidth,'cm'),
      legend.title=element_blank()
    )
  return(gg) }
####

####
# bring in map data
world = fortify(spTransform(getMap(), CRS("+proj=wintri")))
world$cname = countrycode(world$id, 'country.name', 'country.name')

toExclude = c(
  'Greenland', 'Iceland',
  'French Southern and Antarctic Lands',
  'Ashmore and Cartier Islands',
  'Indian Ocean Territories',
  'Siachen Glacier' )

world = world[!world$id %in% toExclude,]
####


####
# bring in dyad data and net estimates
load(paste0(pathIn, 'dyadData.rda'))
####

####
# subset to relev vars
econData = dyadData[,c(
  'cname1','cname2','year',
  'ptaCnt', 'pta',
  'trade', 'tradeDepSend',
  'econScores_tradeDepSend_lfm' 
  )]

# subset to relev timeframe
econData = econData[econData$year>=1990,]

# subset to china 2 cases
china = econData[
  econData$cname2=='CHINA' & econData$cname1!='CHINA', ]
####

####
# create yrs vector to iterate through
yrs = sort(unique(china$year))
yrs = c(1995, 2000, 2005, 2010, 2015, 2020)

# make list of maps
chinaEconMaps = lapply(yrs, function(yr){
  # slice to relev year
  slice = china[china$year==yr,]

  # merge with map data
  slice$rnkVal = rank(-slice$econScores_tradeDepSend_lfm)
  world$val = slice$econScores_tradeDepSend_lfm[
    match(world$cname, slice$cname1)]
  world$rnkVal = slice$rnkVal[
    match(world$cname, slice$cname1)]

  # make map
  map = makeMapRegion(world, 'val', grad2=TRUE) + ggtitle(yr)

  #
  return(map) })

chinaEconMaps
####
