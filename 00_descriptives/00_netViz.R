###Visualization stuff
rm(list=ls())
pth = paste0(here::here(), '/')
source(paste0(pth, 'setup.R'))

#
loadPkg(c(
  'reshape2',
  'ggplot2', 'tidyverse',
  'cshapes', 'countrycode' ))
####

####
load(paste0(pathIn, "dyadData.rda"))
####

####
head(dyadData)

# measure i want to focus on is
# trade_R8_lfm

# as starter code take a look here:
# https://github.com/s7minhas/tensor/blob/master/R/Results/post.R
# starting in line 165
# will also need to take a look at this code to actually generate
# the map
# https://github.com/s7minhas/tensor/blob/master/R/Funcs/ccolors.r
####

####
# a few complications, in that tensor paper we're not using the lfm really
# it's a different model

# and also the lfm dyadic variable is weighted
# and it can be positive or negative, so what we will want are
# weighted edges and we will also want the edges to be colored
# by whether the value of the variable is positive or negative
# my rec for getting a first cut at the weight in igraph is to
# basically weight it by the absolute value of the variable of interest

# so the code wont exactly be a match at all, it's just meant to show
# you how to work with igraph a little bit

# biggest benefit of the code in the tensor git is to show you how
# i generated colors based on the geographic positioning of countries

# but an example of a weighted network in igraph is here
# https://github.com/s7minhas/conflictEvolution

# the general goal that you should have is to generate a network
# depiction of a weighted network where the nodes are colored by
# their geographic position

# and the code should be able to take in different years as inputs
# so that we can generate plots by year
# and also diff measures
####
