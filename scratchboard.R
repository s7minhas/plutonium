####
rm(list=ls())
pth = paste0(here::here(), '/')
pth = 'C:/Users/Owner/Dropbox/Research/booz_allen_gdmm/'
source(paste0(pth, 'setup.R'))

#
library(foreach)
library(doParallel)
####

####
# helper fns

# fn to help determine whether
# a tie between a dyad is sig
# based on int thresholds
binarize = function(vals){
  bin = rep(0, nrow(vals))
  bin[vals$qtLo>0] = 1
  # bin[vals$qtHi<0] = -1
  bin[vals$qtHi<0] = 1
  return(bin) }

# count up number of "sig ties"
sigCnt = function(arrayObject){
  require(reshape2)
  require(dplyr)
	aSumm = arrayObject %>%
    .[,,501:1500] %>% ##modify if chain length changed
		melt(.) %>%
	  filter(Var1 != Var2) %>%
	  mutate(dyad = paste(Var1, Var2, sep='_')) %>%
	  group_by(dyad) %>%
	  summarize(
	    val = mean(value),
	    qtHi = quantile(value, 0.975),
	    qtLo = quantile(value, 0.025)
	  ) %>%
		ungroup() %>%
		mutate(
			bin=binarize(.)
		) %>%
		summarize(
			sigCnt=sum(bin)
		)
	return(aSumm$sigCnt) }
####

####
load(paste0(pth, 'results/diplomScores_agree.rda'))
print('diplom scores data loaded')

cores = 15
cl = makeCluster(cores)
registerDoParallel(cl)
sigCntsDiplom = foreach(
  A = diplomScores,
  .packages=c('dplyr', 'reshape2') ) %dopar% {
  	return( sigCnt(A) )
  } %>% unlist()
stopCluster(cl)

print('sum of diplom sig')
print(sum(sigCntsDiplom)) # 7134 (note the # poss ties is much larger)

rm(diplomScores)
####

####
load(paste0(pth, 'results/econScores_tradeDepSend.rda'))
print('econ scores data loaded')

cores = 15
cl = makeCluster(cores)
registerDoParallel(cl)
sigCntsEcon = foreach(
  A = econScores,
  .packages=c('dplyr', 'reshape2') ) %dopar% {
  	return( sigCnt(A) )
  } %>% unlist()
stopCluster(cl)

print('sum of econ sig')
print(sum(sigCntsEcon)) # 13629

rm(econScores)
####

####
load(paste0(pth, 'results/icewsScores_gov.rda'))
print('icews scores data loaded')

cores = 15
cl = makeCluster(cores)
registerDoParallel(cl)
sigCntsICEWS = foreach(
  A = icewsScores,
  .packages=c('dplyr', 'reshape2') ) %dopar% {
  	return( sigCnt(A) )
  } %>% unlist()
stopCluster(cl)

print('sum of icews sig')
print(sum(sigCntsICEWS)) # 2186

rm(icewsScores)
####
