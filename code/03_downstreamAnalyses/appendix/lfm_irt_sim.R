####
# setup
rm(list=ls())

# pth = paste0(here::here(), '/')
pth = '~/Research/plutonium/code/'
source(paste0(pth, 'setup.R'))

# load pkgs
loadPkg(c('amen', 'MASS'))

# fn to help with procrust trans
procrustes = function(A, B){
  tmp = t(B) %*% A
  svdD = svd(tmp)
  W = svdD$v %*% t(svdD$u)
  Bhat = B %*% W
  return(Bhat) }

#
set.seed(6886)
####

####
# sim params

# num of stats
nStates = 50

# num of un resolutions
nvotes = 300

# lat space dim
dimLFM = 2

# baseline int
beta0 = 0.0

# noise level
sigmaObs = 0.3
####

# gen true pos ###
# generate "true" latent positions for each state
u_true = matrix(
  rnorm(nStates * dimLFM, mean = 0, sd = 1),
  nrow = nStates, ncol = dimLFM )
####

# gen res spec issues ###
# generate resolution-specific "issue" positions
# each resolution also placed in a 2d space
issue_pos = matrix(
  rnorm(nvotes * dimLFM, mean = 0, sd = 1),
  nrow = nvotes, ncol = dimLFM )
####

####
# create individual vote decisions
votes = matrix(NA, nrow = nStates, ncol = nvotes)

# get votes
for(i in 1:nStates){
  for(m in 1:nvotes){
    signal = beta0 + sum(u_true[i, ] * issue_pos[m, ])
    pYes = pnorm(signal, mean = 0, sd = sigmaObs)
    votes[i,m] = rbinom(1, size = 1, prob = pYes) } }
####

####
# aggregate to a co-voting matrix Y (nStates x nStates)
# Y[i,j] = fraction of votes where i and j match
# excluding diags

# set up mat
Y = matrix(NA, nrow = nStates, ncol = nStates)

# fill in
for(i in 1:nStates){
  for(j in 1:nStates){
    if(i == j){
      Y[i,j] = NA
    } else {
      samevotes = sum(votes[i, ] == votes[j, ])
      Y[i,j]    = samevotes / nvotes } } }
####

####
# fit lfm
fit_LFM = ame(
  Y = Y,
  family = "nrm", R = dimLFM,
  symmetric = TRUE, seed=6886, 
  rvar = FALSE, cvar = FALSE,
  burn = 1000, nscan = 5000,
  odens = 10, plot = FALSE,
  print = FALSE, gof = TRUE )
####

####
# extract the estimated latent positions
U_hat = fit_LFM$U

# procrustes transform to align with true positions
U_hat_aligned = procrustes(u_true, U_hat)

# compare true vs. estimated positions
corr_dim1    = cor(u_true[,1], U_hat_aligned[,1])
corr_dim2    = cor(u_true[,2], U_hat_aligned[,2])
corr_overall = cor(as.vector(u_true), as.vector(U_hat_aligned))

# dim 1
round(abs(corr_dim1), 3)
# dim 2
round(abs(corr_dim2), 3)
# dim 3
round(abs(corr_overall), 3)
####

####
# save results
save.image(file=paste0(pathOut, 'lfm_irt_sim.rda'))
####

# ####
# # simulate variant w/ low-discrimination resolutions
# ####

# ####
# # vote setup

# # number of high-discrimination resolutions
# nvotes_hi = 150

# # number of low-discrimination resolutions
# nvotes_lo = 150

# # total number of votes
# nvotes_total = nvotes_hi + nvotes_lo

# # high-discrimination issues (spread in 2d space)
# issue_hi = matrix(
#   rnorm(nvotes_hi * dimLFM, mean = 0, sd = 1),
#   nrow = nvotes_hi, ncol = dimLFM )

# # low-discrimination issues (near 0 → near-consensus)
# issue_lo = matrix(
#   rnorm(nvotes_lo * dimLFM, mean = 0, sd = 0.05),
#   nrow = nvotes_lo, ncol = dimLFM )

# # bind together
# issue_mix = rbind(issue_hi, issue_lo)
# ####

# ####
# # generate votes from mixed issues

# votes_mix = matrix(NA, nrow = nStates, ncol = nvotes_total)

# for(i in 1:nStates){
#   for(m in 1:nvotes_total){
#     signal = beta0 + sum(u_true[i, ] * issue_mix[m, ])
#     pYes = pnorm(signal, mean = 0, sd = sigmaObs)
#     votes_mix[i,m] = rbinom(1, size = 1, prob = pYes) } }
# ####

# ####
# # compute co-voting matrix from mixed votes

# Y_mix = matrix(NA, nrow = nStates, ncol = nStates)

# for(i in 1:nStates){
#   for(j in 1:nStates){
#     if(i == j){
#       Y_mix[i,j] = NA
#     } else {
#       samevotes = sum(votes_mix[i, ] == votes_mix[j, ])
#       Y_mix[i,j] = samevotes / nvotes_total } } }
# ####

# ####
# # fit lfm to mixed matrix

# fit_mix = ame(
#   Y = Y_mix,
#   family = "nrm", R = dimLFM,
#   symmetric = TRUE, seed=6886, 
#   rvar = FALSE, cvar = FALSE,
#   burn = 1000, nscan = 5000,
#   odens = 10, plot = FALSE,
#   print = FALSE, gof = TRUE )
# ####

# ####
# # extract and align
# U_hat_mix = fit_mix$U
# U_hat_mix_aligned = procrustes(u_true, U_hat_mix)

# # compare
# corr_dim1_mix = cor(u_true[,1], U_hat_mix_aligned[,1])
# corr_dim2_mix = cor(u_true[,2], U_hat_mix_aligned[,2])
# corr_overall_mix = cor(as.vector(u_true), as.vector(U_hat_mix_aligned))

# # print results
# round(abs(corr_dim1_mix), 3)
# round(abs(corr_dim2_mix), 3)
# round(abs(corr_overall_mix), 3)
# ####

# ####
# # save variant results
# save(fit_mix, U_hat_mix, U_hat_mix_aligned, 
#      corr_dim1_mix, corr_dim2_mix, corr_overall_mix,
#      file = paste0(pathOut, 'lfm_irt_sim_mixed.rda'))
# ####

