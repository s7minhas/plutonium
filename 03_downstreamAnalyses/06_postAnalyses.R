####
pth = paste0(here::here(), '/')
source(paste0(pth, 'setup.R'))
dpth = paste0(pathDrop, 'data/')
rpth = paste0(pathDrop, 'results/')

#
pkgs = c(
  'lme4', 'rstanarm', 'ggplot2', 'maps', 'patchwork' )
loadPkg(pkgs)

# mod summ fns
digs=3
getCoef = function(x){
  round(cbind(mu=fixef(x), sd=sqrt(diag(vcov(x)))),digs) }

getCoefB = function(x, vars, int = FALSE){
  out = cbind(
    mu=apply(x[,vars], 2, mean) ,
    sd=apply(x[,vars], 2, sd) )
  if(int){
    out = cbind(out,
      qtLo95 = apply(x[,vars], 2, quantile, .025),
      qtHi95 = apply(x[,vars], 2, quantile, .975) ) }
  out = round( out, digs )
  return(out) }
####

####
# load data and results
# modData, m1, m2, m3, m1b, m2b, m3b
load(paste0(rpth, 'dstreamModels_cname_v2.rda'))
####

####
ivs = c(
  '(Intercept)',
  'USf1.l1',
  'USf2.l1',
  'polity.l1',
  'GDP.l1', 'pop.l1',
  # 'beijDist',
  'IdealPointDistance' )
####

####
# check convergence
plot(bf1, 'trace', pars=ivs[-3])
plot(bf2, 'trace', pars=ivs[-2])
plot(bf1HetEff, 'trace', pars=ivs[-(2:3)])
plot(bf2HetEff, 'trace', pars=ivs[-(2:3)])
####

####
# pull out mats
bMats = lapply(
  list(bf1, bf2, bf1HetEff, bf2HetEff),
  function(x){ as.matrix(x) } )
names(bMats) = c('bf1', 'bf2', 'bf1HetEff', 'bf2HetEff')
####

####
#  quick comparison of iv effs
getCoef(mf1)
getCoefB(bMats$'bf1', ivs[-3])

getCoef(mf2)
getCoefB(bMats$'bf2', ivs[-2])

getCoef(mf1HetEff)
getCoefB(bMats$'bf1HetEff', ivs[-(2:3)])

getCoef(mf2HetEff)
getCoefB(bMats$'bf2HetEff', ivs[-(2:3)])
####

####
# ran effects
effs = setdiff(colnames(bMats$'bf1HetEff'), ivs[-(2:3)])
effs = effs[!grepl('(Intercept)', effs)]
f1HetEffs = getCoefB(bMats$'bf1HetEff', effs, T)

effs = setdiff(colnames(bMats$'bf2HetEff'), ivs[-(2:3)])
effs = effs[!grepl('(Intercept)', effs)]
f2HetEffs = getCoefB(bMats$'bf2HetEff', effs, T)
####

####
# ran eff map
hetEffs = list(f1=f1HetEffs, f2=f2HetEffs)
hetEffs = lapply(hetEffs, function(x){
  df = data.frame(x, row.names=NULL)
  cntry = rownames(x)
  cntry = gsub('b[USf1.l1 cname1:','',cntry,fixed=TRUE)
  cntry = gsub(']', '', cntry, fixed=TRUE)
  cntry = trim(cntry)
  cntry = countrycode(cntry, 'country.name', 'country.name')
  df = cbind(cntry=cntry, df)
  df$sig = ((df$qtLo95*df$qtHi95)>0)*sign(df$mu)
  return(df) })

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

# maps
makeMap = function(dat, var, cat=FALSE){

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
      legend.key.width=unit(1,'cm') )

  #
  return(gg) }

# apply fns
f1map = makeMap(world, 'f1_mu', F)
f2map = makeMap(world, 'f2_mu', F)
f1sigmap = makeMap(world, 'f1_sig', T)
f2sigmap = makeMap(world, 'f2_sig', T)

#
maps = (f1map + f2map) / (f1sigmap + f2sigmap)
maps
ggsave(maps, file=paste0(rpth, 'hetEffMaps_v2.pdf'))
####
