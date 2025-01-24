if(Sys.info()['user'] %in% c('sminhas')){
	suf = '~/'
	pathGit = paste0(suf, 'Research/plutonium/')
	pathPaper = paste0(pathGit, 'Paper/')
	pathCode = paste0(pathGit, 'code/')
	pathFuncs = paste0(pathCode, 'funcs/')
	pathDrop = paste0('/media/sminhas/localCloud/Dropbox/Research/plutonium/')
	pathIn = paste0(pathDrop, 'data/')
	pathOut = paste0(pathDrop, 'results/')
	pathGraphics = paste0(pathDrop, 'graphics/')
}

if(Sys.info()['user'] %in% c('maxgallop')){
	suf = paste0('/Users/', Sys.info()['user'], '/')
	pathDrop = paste0(suf, 'Dropbox/plutonium/')
	pathIn = paste0(pathDrop, 'data/')
	pathOut = paste0(pathDrop, 'results/')
  pathGraphics = paste0(pathDrop, 'graphics/')
}

if(Sys.info()['user'] %in% c('haeunchoi')){
  suf = paste0('/Users/', Sys.info()['user'], '/')
  pathDrop = paste0(suf, 'Dropbox/plutonium/')
  pathIn = paste0(pathDrop, 'data/')
  pathOut = paste0(pathDrop, 'results/')
  pathGraphics = paste0(pathDrop, 'graphics/')
}

# General functions/libraries
## See info on package versions and other session info
### at bottom of script
loadPkg=function(toLoad){
	for(lib in toLoad){
	  if(!(lib %in% installed.packages()[,1])){
	    install.packages(lib, repos='http://cran.rstudio.com/') }
	  library(lib, character.only=TRUE)
	}
}

toLoad=c(
	'countrycode',
	'reshape2', 'tidyr', 'dplyr', 'abind'
	)
loadPkg(toLoad)

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

## system and package information
# sessionInfo()
# R version 4.0.3 (2020-10-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19042)
#
# Matrix products: default
# locale:
# [1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252
# [3] LC_MONETARY=English_United States.1252 LC_NUMERIC=C
# [5] LC_TIME=English_United States.1252
#
# locale:
# [1] C
#
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base
#
# loaded via a namespace (and not attached):
# [1] compiler_4.0.3 tools_4.0.3

# packs = c(
# 	'countrycode', 'reshape2', 'tidyr', 'dplyr', 'abind',
# 	'ggplot2', 'tidyverse', 'ggplot2', 'extrafont', 'Cairo',
# 	'RColorBrewer', 'doParallel', 'foreach', 'amen', 'WDI',
# 	'readr', 'imfr', 'here'
# )
# packs = sort(packs)
# info = installed.packages()[packs,'Version']
# toTab = paste0(packs, ': ', info)
# names(toTab) = NULL
# toTab = c(toTab, '', '')
# toTab = matrix(toTab, nrow=5, ncol=4, byrow=TRUE)
# knitr::kable(toTab)
# |                    |               |                |                  |
# |:-------------------|:--------------|:---------------|:-----------------|
# |abind: 1.4-5        |amen: 1.4      |Cairo: 1.5-12   |countrycode: 0.16 |
# |doParallel: 1.0.15  |dplyr: 0.8.5   |extrafont: 0.17 |foreach: 1.5.0    |
# |ggplot2: 3.3.0      |ggplot2: 3.3.0 |here: 1.0.1     |imfr: 0.1.9.1     |
# |RColorBrewer: 1.1-2 |readr: 1.3.1   |reshape2: 1.4.4 |tidyr: 1.0.2      |
# |tidyverse: 1.3.0    |WDI: 2.7.0     |                |                  |
