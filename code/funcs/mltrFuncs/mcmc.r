### note mcmc function depends on the following
### scripts
# "tfunctions.r"
# "functions_als.r"
# "functions_bayes.r"
### and packages
# abind

mltr = function(
  Y,
  X,
	NS = 1000,
  plot = FALSE,
	seed = 6886,
  rstart = FALSE
){

# seed and dims
set.seed(seed)
K=length(dim(X))-1

## starting values if not random
if(!rstart)
{
  B=BMLE=mlm.ALS(Y,X,imax=5,verbose=TRUE)
  YI=Y ; YI[is.na(Y)]=tprod(X,B)[is.na(Y)]
  R=YI-tprod(X,B)
  S=list()
  for(k in 1:K)
  {
    S[[k]]=tcrossprod(mat(R,k))
    S[[k]]=S[[k]]*nrow(S[[k]])/tr(S[[k]])
  }
  s2=mean(R^2)
}
##

## random starting values
if(rstart)
{
  B=S=list()
  for(k in 1:K)
  {
    B[[k]]=rsan(c(dim(Y)[k],dim(X)[k]))
    S[[k]]=solve(rwish(diag(nrow=dim(Y)[k])))
  }
  s2=rgamma(1,1,1)
  YI=Y ; YI[is.na(Y)]=tprod(X,B)[is.na(Y)]
}
####

#### objects to store output
MSE=S2=NULL ; BPS=SPS=list()
for(k in 1:(length(dim(Y))-1))
{
  BPS[[k]]=array(dim=c(dim(Y)[k],dim(X)[k],0))
  SPS[[k]]=array(dim=c(dim(Y)[k],dim(Y)[k],0))
}
####

#### MCMC
for(s in 1:NS)
{

  ## Gibbs update
  BS=rBSa_fc(YI,X,B,S) ; B=BS$B ; S=BS$S ; s2=BS$s2
  ##

  ## impute missing data
  XB=tprod(X,B)
  YI[is.na(Y)]=( XB+sqrt(s2)*tprod(rsan(dim(Y)),lapply(S,mhalf)))[is.na(Y)]
  ##

  ## output
  SS2=S_scaled(S,s2)
  S2=c(S2,SS2$s2)
  BPS=mapply( abind, BPS, B_scaled(B) )
  SPS=mapply( abind, SPS, SS2$S )
  MSE=c(MSE,mean( (YI-XB)^2 ) )

  # plot to check on diogs for the model results
  if(plot)
  {
    par(mfrow=c(2,2))
    plot(MSE,type="l")
    plot(S2,type="l")
    for(k in 3:K)
    {
      matplot(t(apply(BPS[[k]],c(3),"c")),type="l")
      abline(h=0,lwd=2,col="gray")
    }
  }
  ##
}
#### end mcmc

#### organize and return output

# add labels
dimnames(BPS[[1]])[1:2] = dimnames(BPS[[2]])[1:2] = dimnames(Y)[1:2]

#
out = list(
  BPS=BPS, SS2=SS2,
  S2=S2, SPS=SPS, MSE=MSE )

#
return(out)
####

} # end function
