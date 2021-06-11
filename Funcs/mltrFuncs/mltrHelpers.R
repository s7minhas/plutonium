#### Data prep
# Cast directed dyadic variable into an array expected by mltr
# note that when using this function any NAs will be converted
# to zero, main purpose of this is to set diagonals to zero
# transform to array
prepDataMLTR = function(
  dyadYearData,
  sendID, recID, timeID, varNames,
	qtTrans=TRUE
){
  require(tidyr)
  require(reshape2)

  # rearrange df and relabel vars
  dyadYearData = dyadYearData[,c(sendID, recID, timeID, varNames)]
  names(dyadYearData)[1:3] = c('sender', 'receiver', 'time')

  # transform data into long format
  longData = tidyr::pivot_longer(
    data=dyadYearData, cols=all_of(varNames),
    names_to='variable', values_to='value' )

  # cast data into an array
  Z = acast(
    data=longData,
    formula=sender~receiver~variable~time, value.var='value' )

  # set all NAs to zero
  Z[is.na(Z)] = 0

  # apply qt-qt transform
	if(qtTrans){
		Z <- aperm(apply(Z,c(1,2,3),zscores) ,c(2,3,4,1))
	}

  # prep vars to stabilize estimates
  ZM = Z # ij, main effect
  ZR = aperm(Z, c(2, 1, 3, 4)) # ji, reciprocal effect
  ZT = Z # ijk, transitive effect
  for(vv in 1:dim(ZT)[3]){
    for(tt in 1:dim(ZT)[4]){
      XS = ( ZT[,,vv,tt] + t(Z[,,vv,tt]) )/2
      ZT[,,vv,tt] = XS %*% XS } }

  # cleanup and organize
  # note that we have to drop the first time obs in order
  # to create lagged versions, which is necessary to
  # properly estimate the bilinear parameters
  Y = Z[,,,-1]

  # organize endog terms
  X = array( dim=append( dim(Y), 3, after=3))
  X[,,,1,] = ZM[,,,-dim(Z)[4]]
  X[,,,2,] = ZR[,,,-dim(Z)[4]]
  X[,,,3,] = ZT[,,,-dim(Z)[4]]

  # clean up x array
  X = aperm(apply(X, c(1,2,5), 'c'), c(2,3,1,4))

  # clean up varnames
  dimnames(X)[1:2] = dimnames(Y)[1:2]
  dimnames(X)[[4]] = dimnames(Y)[[4]]
  dimnames(X)[[3]] = apply(
    expand.grid( dimnames(Y)[[3]],
    c('ij','ji','ijk')),1,paste,collapse='_')

  #
  return( list(Y=Y, X=X) )
}

# function to create a list of arrays from
# frame, this function has not been generalized
# will only reliably work when doing manipulations
# from the frame object
getListArrays = function(
  timePd, timeRange, idVars, valVars, qtApply
){
  # get list of arrays
  arrList = lapply(timePd, function(tt){

    # identify range of yrs
    tRange = (tt-(timeRange)):tt

    # subset by range of years and relev vars
    slice = frame[frame$year %in% tRange,c(idVars, valVars)]

    # identify countries that exist in every year of the five year period
    cntries=lapply(tRange, function(tt){
      ttData = slice[slice$year==tt,]
      ttCntries = unique( c( ttData$cname1, ttData$cname2 ) )
      return(ttCntries)
      }) %>% Reduce('intersect', .)

    # subset data by vector of countries
    slice = slice[slice$cname1 %in% cntries,]
    slice = slice[slice$cname2 %in% cntries,]

    # get array
    out = prepDataMLTR(
      dyadYearData = slice,
      sendID = 'cname1',
      recID = 'cname2',
      timeID = 'year',
      varNames = names(slice)[5:ncol(slice)],
			qtTrans=qtApply
    )

    #
    return(out) })

  #
  names(arrList) = timePd
  return(arrList) }
