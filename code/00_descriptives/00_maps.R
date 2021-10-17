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
makeMapRegion = function(
  dat, var, legWidth=1,
  grad2=TRUE, discScale=FALSE){

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
    gg = gg + scale_fill_viridis(discrete=discScale) }

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

cntryMaps = function(
  cntry, yrBrks, var,
  ddata=econData, cnameRef=2, rank=FALSE,
  gradLogic=TRUE, discreteVar=FALSE){

  # desig ref var
  ddata$ref = ddata[,paste0('cname', cnameRef)]
  ddata$oth = ddata[,paste0('cname',setdiff(1:2,cnameRef))]
  ddata$othcode = countrycode(ddata$oth, "country.name", "cown")
  # desig var for easier ref
  ddata$val = ddata[,var]

  # subset to cntry data
  cntryData = ddata[ ddata$ref==cntry & ddata$oth!=cntry, ]

  # make list of maps by year breaks
  cntryEconMaps = lapply(yrBrks, function(yr){

    # slice to relev year
    slice = cntryData[cntryData$year==yr,]

    # create ranked version
    slice$rnkVal = rank(-slice$val)

    # merge with map data
    world$val = slice$val[match(world$ccode, slice$othcode)]
    world$rnkVal = slice$rnkVal[match(world$ccode, slice$othcode)]

    # choose version of var to plot
    if(!rank){
      world$toPlot = world[,'val']
    } else { world$toPlot = world[,'rnkVal'] }

    # make map
    map = makeMapRegion(
      world, 'toPlot',
      grad2=gradLogic, discScale=discreteVar)

    #
    return(map) })

  #
  return(cntryEconMaps) }
####

cntry = "UNITED STATES"
var = "diplomScores_agree_lfm"
yr = 2019
ggsave(paste0(pathGraphics, "USdiplom2019.pdf"), map)
####
# bring in map data
world = fortify(spTransform(getMap(), CRS("+proj=wintri")))
world$cname = countrycode(world$id, 'country.name', 'country.name')
world$ccode = countrycode(world$id, 'country.name', 'cown')

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
  'treatyBin_R2_lfm', "icewsScores_gov_lfm", "diplomScores_agree_lfm"
  )]

econData$ccode1 = countrycode(econData$cname1, "country.name", "cown")
econData$ccode2 = countrycode(econData$cname2, "country.name", "cown")

# subset to relev timeframe
econData = econData[econData$year>=1990,]

usEcon = cntryMaps(
  cntry = cname('UNITED STATES'),
  yrBrks = c(1995, 2000, 2005, 2010, 2015, 2020),
  var = 'treatyBin_R2_lfm' )

chinaEcon = cntryMaps(
  cntry = cname('CHINA'),
  yrBrks = c(1995, 2000, 2005, 2010, 2015, 2020),
  var = 'treatyBin_R2_lfm' )


usDiplo = cntryMaps(
  cntry = cname('UNITED STATES'),
  yrBrks = c(1995, 2000, 2005, 2010, 2015, 2019),
  var = 'diplomScores_agree_lfm' )

chinaDiplo = cntryMaps(
  cntry = cname('CHINA'),
  yrBrks = c(1995, 2000, 2005, 2010, 2015, 2019),
  var = 'diplomScores_agree_lfm' )



#### Put in a number for the map for that year
chinaDiplo[[length(chinaDiplo)]]
usDiplo[[length(usDiplo)]]
####
