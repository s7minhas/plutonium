if(Sys.info()['user'] %in% c('S7M','s7m','herme','Owner')){
	suf = paste0('C:/Users/', Sys.info()['user'], '/')
	pathDrop = paste0(suf, 'Dropbox/Research/plutonium/')
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

# ts helpers
lagger<-function(variable, country, year, laglength){

  country<-as.character(country)
  laggedvar<-rep(NA,length(variable))

  leadingNAs<-rep(NA,laglength)
  countryshift<-c(leadingNAs, country[1:(length(country)-laglength)])

  variableshift<-c(leadingNAs, variable[1:(length(variable)-laglength)])

  replacementrefs<-country==countryshift
  replacementrefs[is.na(replacementrefs)==T]<-FALSE
  laggedvar[replacementrefs]<-variableshift[replacementrefs]

  laggedvar

} # close lagger function

# function for lagging whole dataframes:
multilagger<-function(X, country, year, laglength, relabel=T){

  if(is.data.frame(X)==F) stop("X needs to be a dataframe")

  laggedX<-X

  for (i in 1:ncol(X)){

    laggedX[,i]<-lagger(variable=X[,i], country=country, year=year, laglength=laglength)

  } # close i loop

  # now append the laglength to the variable names:
  if (relabel==T){
    suffix<-paste(".l",laglength,sep="")
    names(laggedX)<-paste(names(X), suffix, sep="")
  } # close if relabel==T condition

  laggedX

} # close multilagger function

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
