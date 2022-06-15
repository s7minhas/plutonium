####
pth = paste0(here::here(), '/')
source(paste0(pth, 'setup.R'))

#
pkgs = c(
	'ggplot2', 'patchwork',
	'extrafont', 'Cairo',
	'rnaturalearth', 'rnaturalearthdata' )
loadPkg(pkgs)

#
source(paste0(pathFuncs, 'modSummHelpers.R'))
####

####
load(paste0(pathOut, 'modelInfoFin_russia.rda'))
load(paste0(pathOut, 'lmerModsFin_russia.rda'))
####

####
# mods with cntry effs
cMods = lmerMods[grepl('cname1',names(lmerMods))]

# org into df
cEff = lapply( 1:length(cMods), function(ii){
	eff = cMods[[ii]]$effects
	eff = eff[eff$grpvar=='cname1',]

	#
	lab = names(cMods)[ii]
	eff$model = lab
	eff$dv = unlist(lapply(strsplit(lab, '_'), function(x){x[1]}))
	eff$kiv = unlist(lapply(strsplit(lab, '_'), function(x){x[2]}))

	#
	eff$modLab = eff$model
	eff$modLab[grepl('f1',eff$modLab)] = 'F1 (Active US Conflicts)'
	eff$modLab[grepl('f2',eff$modLab)] = 'F2 (US Defense Spending)'

	#
	eff = eff[,-match(c('grpvar','term','t value'), names(eff))]

	#
	return(eff) } )
####

####
# get world map shape file
world <- ne_countries(scale = "medium", returnclass = "sf")

# remove dependencies
world = world[world$sovereignt == world$admin,]

# fix greenland coding
world$sovereignt[world$adm0_a3=='GRL'] = 'Greenland'
world = world[grepl('ountry', world$type),]

# remove extra cyprus obs
world = world[world$sovereignt!='Northern Cyprus',]

# add country label for matching
world$cname = cname(world$sovereignt)

# get eff scores
cEff = cEff[c(1,2)]
mapData = do.call('rbind', lapply(cEff, function(eff){

	# merge in country eff from mod
	world$eff = eff$Estimate[match(world$cname, eff$iv)]

	# dv and mod
	world$dv = unique(eff$dv)
	world$mLab = unique(eff$modLab)

	#
	return(world) }) )

#
mapData$eLab = mapData$dv
mapData$eLab[mapData$dv=='agree'] = "Country Random Effects from China Diplomatic Alignment Model"
mapData$eLab[mapData$dv=='tradeDepSend'] = "Country Random Effects from China Trade Alignment Model"

#
eMaps = ggplot(data = mapData) +
    geom_sf(aes(fill=eff), color='grey50', size=.1) +
    coord_sf(crs = "+proj=moll") +
		scale_x_continuous(expand = c(0,0)) +
		scale_y_continuous(expand = c(0,0)) +
		scale_fill_gradient2() +
 		labs(fill='') +
		facet_wrap(~eLab, nrow=2) +
		theme_minimal() +
		theme(
			axis.text=element_blank(),
			legend.position='top',
      legend.key.width = unit(1, 'cm'),
			panel.grid=element_blank(),
      strip.text.x = element_text(
        size = 9, color='white',
        family="Source Sans Pro Semibold"),
    	strip.background = element_rect(fill = "#525252", color='#525252') )

#
ggsave(eMaps,
	file=paste0(pathPaper, 'eMaps_russia.pdf'),
	width=8, height=6, device=cairo_pdf )

# arrange horizontally
eMaps2 = eMaps +
	facet_wrap(~eLab, nrow=1)

#
ggsave(eMaps2,
	file=paste0(pathPaper, 'eMapsv2_russia.pdf'),
	width=8, height=3, device=cairo_pdf )
####
