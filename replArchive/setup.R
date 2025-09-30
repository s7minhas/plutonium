####################################
# setup
####################################

# set base path for repl archive
bpth = './'  # base path - current dir for repl

# define path vars
dpth = paste0(bpth, 'data/')        # data dir
rpth = paste0(bpth, 'results/')     # results dir
gpth = paste0(bpth, 'graphics/')    # graphics dir
rfuncs = paste0(bpth, 'funcs/')     # funcs dir

# create dirs if they don't exist
dir.create(dpth, recursive = TRUE, showWarnings = FALSE)
dir.create(rpth, recursive = TRUE, showWarnings = FALSE)
dir.create(gpth, recursive = TRUE, showWarnings = FALSE)
dir.create(rfuncs, recursive = TRUE, showWarnings = FALSE)

# load pkgs func
loadPkg = function(toLoad){
  for(lib in toLoad){
    if(!(lib %in% installed.packages()[,1])){
      install.packages(lib, repos='http://cran.rstudio.com/') }
    suppressMessages(suppressWarnings(library(lib, character.only=TRUE)))
  }
}

#
toLoad=c(
	'countrycode',
	'reshape2', 'tidyr', 'dplyr', 'abind'
	)
loadPkg(toLoad)

####################################
# utility funcs
####################################

cname = function(x){countrycode(x, 'country.name','country.name')}
char = function(x){ as.character(x) }
num = function(x){ as.numeric(char(x)) }
trim = function (x) { gsub("^\\s+|\\s+$", "", x) }
substrRight = function(x, n){ substr(x, nchar(x)-n+1, nchar(x)) }
sumNA = function(x){
  if (all(is.na(x))){
    NA
  } else {
    sum(x, na.rm = TRUE)
  }
}
extractFromID = function(x, pos){
	out = strsplit(x, '_')
	out = lapply(out, function(x){x[pos]})
	return(unlist(out))
}
pasteVec = function(x, y, sep=''){
	combos=expand.grid(x, y)
	apply(combos, 1, paste, collapse=sep)
}
stdz = function(x){ (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE) }
cbindNumb = function(x){ cbind(x, 1:length(x)) }
recodeQt = function(var, pseq){
  qts = quantile(var, probs=pseq, na.rm=TRUE)
  qts[1] = -Inf
  qts = unique(qts)
  qtLabs = paste0('cat_', 1:(length(qts)-1))
  qtCat = cut(var, qts, qtLabs)
  return(qtCat) }
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


# Global params
seed=6886
set.seed(seed)

####################################