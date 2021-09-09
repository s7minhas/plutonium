####
pth = paste0(here::here(), '/')
source(paste0(pth, 'setup.R'))
dpth = paste0(pathDrop, 'data/')
rpth = paste0(pathDrop, 'results/')

#
pkgs = c(
  'lme4', 'rstanarm', 'ggplot2', 'maps', 'patchwork' )
loadPkg(pkgs)
####

####
# load data and results
# modData, m1, m2, m3, m1b, m2b, m3b
load(paste0(rpth, 'dstreamModels_cname.rda'))
####

####
ivs = c(
  '(Intercept)',
  'USf1.l1',
  'USf2.l1',
  'polity.l1',
  'GDP.l1', 'pop.l1',
  'beijDist',
  'IdealPointDistance' )
####

####
# pull out mats
bMats = lapply(
  list(bf1, bf2, bf1HetEff, bf2HetEff),
  function(x){ as.matrix(x) } )
names(bMats) = c('bf1', 'bf2', 'bf1HetEff', 'bf2HetEff')
####

####
# ran effects
effs = setdiff(colnames(bMats$'bf1HetEff'), ivs[-(2:3)])
f1effs = effs[!grepl('(Intercept)', effs)]
inteffs = effs[grepl('(Intercept)', effs)]
f1HetEffs = getCoefB(bMats$'bf1HetEff', f1effs, T)
int1HetEffs = getCoefB(bMats$'bf1HetEff', inteffs, T)


effs = setdiff(colnames(bMats$'bf2HetEff'), ivs[-(2:3)])
f2effs = effs[!grepl('(Intercept)', effs)]
inteffs = effs[grepl('(Intercept)', effs)]
f2HetEffs = getCoefB(bMats$'bf2HetEff', f2effs, T)
int2HetEffs = getCoefB(bMats$'bf2HetEff', inteffs, T)
####

####
# ran eff map

# org heter eff data
hetEffs = list(f1HetEffs, f2HetEffs)
hetEffs = lapply(1:length(hetEffs), function(ii){
  x = hetEffs[[ii]]
  df = data.frame(x, row.names=NULL)
  cntry = rownames(x)
  cntry = gsub(paste0('b[USf',ii,'.l1 cname1:'),'',cntry,fixed=TRUE)
  cntry = gsub(']', '', cntry, fixed=TRUE)
  cntry = trim(cntry)
  cntry = countrycode(cntry, 'country.name', 'country.name')
  df = cbind(cntry=cntry, df)
  df$sig = ((df$qtLo95*df$qtHi95)>0)*sign(df$mu)
  return(df) })
names(hetEffs) = c('f1', 'f2')

# org heter eff data
intEffs = list(int1HetEffs, int2HetEffs)
intEffs = lapply(1:length(intEffs), function(ii){
  x = intEffs[[ii]]
  df = data.frame(x, row.names=NULL)
  cntry = rownames(x)
  cntry = gsub('b[(Intercept) cname1:','',cntry,fixed=TRUE)
  cntry = gsub(']', '', cntry, fixed=TRUE)
  cntry = trim(cntry)
  cntry = countrycode(cntry, 'country.name', 'country.name')
  df = cbind(cntry=cntry, df)
  df$sig = ((df$qtLo95*df$qtHi95)>0)*sign(df$mu)
  return(df) })
names(intEffs) = c('i1', 'i2')

# bring in map data
world = map_data("world")
world$cname = countrycode(world$region, 'country.name', 'country.name')
world = world[!is.na(world$cname),]
world = world[world$cname!='Greenland',]

# merge in hetEff data
toMerge = names(hetEffs[[1]])[-1]
mods = names(hetEffs)
for(mod in mods){
  slice = hetEffs[[mod]]
  for(v in toMerge){
    world$tmp = slice[match(world$cname, slice$cntry),v]
    names(world)[ncol(world)] = paste0(mod,'_',v) } }

# merge in intEff data
toMerge = names(intEffs[[1]])[-1]
mods = names(intEffs)
for(mod in mods){
  slice = intEffs[[mod]]
  for(v in toMerge){
    world$tmp = slice[match(world$cname, slice$cntry),v]
    names(world)[ncol(world)] = paste0(mod,'_',v) } }

world = na.omit(world)
head(world)
cor(world$f1_mu, world$i1_mu) # that's weird

# maps
makeMap = function(dat, var, cat=FALSE, legWidth=1){

  # desig colVar
  dat$colVar = dat[,var]

  # set up gg frame
  gg = ggplot(dat, aes(x=long, y=lat, group=group))

  # desig what should happen if cat or not
  if(!cat){
    gg = gg +
      geom_polygon(
        aes(group=group, fill=colVar),
        color='grey20', size=.1 ) +
      scale_fill_gradient2() }
  if(cat){
    gg = gg +
      geom_polygon(
        aes(group=group, fill=factor(colVar)),
        color='grey20', size=.1 ) +
      scale_fill_manual(values=c('#e41a1c', '#f0f0f0', '#377eb8')) }

  # cleanup plot
  gg = gg +
    coord_map(xlim=c(-180,180), ylim=c(-50,80)) +
    theme_void() +
    theme(
      legend.position='top',
      legend.key.width=unit(legWidth,'cm') )

  #
  return(gg) }

# apply fns
f1map = makeMap(world, 'f1_mu', F, 1)
f2map = makeMap(world, 'f2_mu', F, 1)
f1sigmap = makeMap(world, 'f1_sig', T, .5)
f2sigmap = makeMap(world, 'f2_sig', T, .5)

# apply fns
i1map = makeMap(world, 'i1_mu', F, 1)
i2map = makeMap(world, 'i2_mu', F, 1)
i1sigmap = makeMap(world, 'i1_sig', T, .5)
i2sigmap = makeMap(world, 'i2_sig', T, .5)

#
maps = (f1map + f2map) / (f1sigmap + f2sigmap)
ggsave(maps, file=paste0(rpth, 'hetEffMaps.pdf'))

#
imaps = (i1map + i2map) / (i1sigmap + i2sigmap)
ggsave(maps, file=paste0(rpth, 'intEffMaps.pdf'))
####
