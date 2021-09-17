####
#
library(ggplot2)
library(maps)
library(patchwork)
library(countrycode)
####

####
# bring in map data
world = map_data("world")
world$cname = countrycode(world$region, 'country.name', 'country.name')
world = world[!is.na(world$cname),]
world = world[world$cname!='Greenland',]
####

####
# fn to make maps
makeMapRegion = function(dat, var, legWidth=1){

  # desig colVar
  dat$colVar = dat[,var]

  # set up gg frame
  gg = ggplot(dat, aes(x=long, y=lat, group=group)) +
      geom_polygon(
        aes(group=group, fill=factor(colVar)),
        color='grey20', size=.1 ) +
    coord_map(xlim=c(-180,180), ylim=c(-50,80)) +
    theme_void() +
    theme(
      legend.position='none'
     )

  #
  return(gg) }
####

####
# add region groupings to countries
world$regionAgg = countrycode(world$cname, 'country.name', 'region')
world$regionAgg_23 = countrycode(world$cname, 'country.name', 'region23')
####

####
# make region map
regMap = makeMapRegion(world, 'regionAgg', .3)
reg23Map = makeMapRegion(world, 'regionAgg_23', .3)
####

####
# org maps for printing
maps = regMap + reg23Map
maps
####

####
# spit out csv with region groupings as well
toShare = unique(world[,c('cname', 'regionAgg', 'regionAgg_23')])
head(toShare)
####
