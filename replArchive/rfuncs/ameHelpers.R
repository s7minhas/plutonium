##################################################################
loadPkg('ggrepel')

getDataForCirc = function(
	Y, U=NULL, V=NULL, row.names=rownames(Y),
	col.names=colnames(Y), vscale=.8, removeIsolates=TRUE,
	uLabel='U', vLabel='V'
	){

	#
	vLogic = is.null(V) ; uLogic = is.null(U)

	if (uLogic) {
	    a <- rowMeans(Y, na.rm = TRUE)
	    b <- colMeans(Y, na.rm = TRUE)
	    Y0 <- Y
	    Y0[is.na(Y)] <- (outer(a, b, "+"))[is.na(Y)]
	    Y0 <- Y0 - mean(Y0)
	    if (!all(Y == t(Y), na.rm = TRUE)) {
	        sY <- svd(Y0)
	        u <- sY$u[, 1:2]
	        v <- sY$v[, 1:2]
	        mu <- sqrt(apply(u^2, 1, sum))
	        mv <- sqrt(apply(v^2, 1, sum))
	        u <- diag(1/mu) %*% u
	        v <- diag(1/mv) %*% v * vscale
	    }
	    if (all(Y == t(Y), na.rm = TRUE)) {
	        eY <- eigen(Y0)
	        bv <- which(abs(eY$val) >= sort(abs(eY$val), decreasing = TRUE)[2])[1:2]
	        u <- eY$vec[, bv]
	        mu <- sqrt(apply(u^2, 1, sum))
	        u <- diag(1/mu) %*% u
	        mv <- mu
	        v <- u
	    }
	}
	if (!uLogic) {
	    if (vLogic) {
	        V <- U
	        vscale <- 1
	    }
	    mu <- sqrt(apply(U^2, 1, sum))
	    mv <- sqrt(apply(V^2, 1, sum))
	    u <- diag(1/mu) %*% U
	    v <- diag(1/mv) %*% V * vscale
	}

	rsum <- apply(abs(Y), 1, sum, na.rm = TRUE)
	csum <- apply(abs(Y), 2, sum, na.rm = TRUE)
	links <- which(Y != 0, arr.ind = TRUE)

	# org df for gg
	uG = data.frame(u*1.2)
	uG$actor = rownames(Y)
	uG$tPch = 0 ; uG$tPch[rsum>0] = (mu[rsum>0])^3
	if(removeIsolates){ uG = uG[uG$tPch>0,] }
	uG$tPch = uG$tPch

	# add v if supplied
	if(!vLogic){
		vG = data.frame(v*1.2)
		vG$actor = rownames(Y)
		vG$tPch = 0 ; vG$tPch[csum>0] = (mv[csum>0])^3
		if(removeIsolates){ vG = vG[vG$tPch>0,] }
		vG$tPch = vG$tPch

		uG$eff = uLabel ; vG$eff = vLabel
		uG = rbind(uG, vG)
		uG$eff = factor(uG$eff, levels=c(uLabel,vLabel)) }

	#
	out = list(uG=uG, U=U, V=V, links=links, u=u, v=v)
}

ggCirc = function(
	Y, U=NULL, V=NULL, row.names=rownames(Y), col.names=colnames(Y),
	vscale=.8, prange=c(2,5), lcol='gray85', ltype='dotted', lsize=.5,
	force=1, maxIter = 3e3, removeIsolates=TRUE, uLabel='U', vLabel='V',
	showActLinks=TRUE, geomLabel=TRUE, geomText=FALSE, geomPoint=TRUE, ...
	){

	#
	ggData = getDataForCirc(Y=Y, U=U, V=V,
		row.names=row.names, col.names=col.names,
		vscale=vscale, removeIsolates=removeIsolates,
		uLabel=uLabel, vLabel=vLabel)
	uG=ggData$uG ; U=ggData$U ; V=ggData$V
	links=ggData$links ; u=ggData$u ; v=ggData$v

	#
	vLogic = is.null(V) ; uLogic = is.null(U)

	# add v if supplied
	if(!vLogic){ ggCirc = ggplot(uG, aes(x=X1, y=X2, color=eff)) }
	if(vLogic){ ggCirc = ggplot(uG, aes(x=X1, y=X2)) }

	# add segments
	if(showActLinks){
		for(i in 1:nrow(links)){
			ggCirc = ggCirc + geom_segment(
				x=u[links[i,1],1]*1.2, y=u[links[i,1],2]*1.2,
				xend=v[links[i,2],1]*1.2, yend=v[links[i,2],2]*1.2,
				color=lcol, linetype=ltype, size=lsize ) } }
	if(geomPoint){ ggCirc = ggCirc + geom_point() }
	if(geomLabel){ ggCirc = ggCirc + geom_label_repel(aes(label=actor, size=tPch, ...),
		force=force, max.iter=maxIter) }
	if(geomText){ ggCirc = ggCirc + geom_text_repel(aes(label=actor, size=tPch, ...),
		force=force, max.iter=maxIter) }
	ggCirc = ggCirc + scale_size(range=prange) +
		theme(
			legend.position='none',
			axis.ticks=element_blank(),
			axis.title=element_blank(),
			axis.text=element_blank(),
			panel.border=element_blank(),
			panel.grid=element_blank()
			)
	return(ggCirc)
}

uvViz = function(
  catSelect,
  configSelect,
  paramsToPlot,
  timeSelect,
	cntryVec
){
  # pick input
  if(catSelect=='Trade'){ dat=tradeMods }
  if(catSelect=='UN Voting'){ dat=unMods }

  # subet to relev config, param, and time
  dat = dat[[configSelect]][[char(timeSelect)]][c('YPM','U', 'V')]

  # modify labels
  ids=cntryKey$cowc[match(rownames(dat$YPM), cntryKey$cname)]
  rownames(dat$YPM) = colnames(dat$YPM) = rownames(dat$U) = rownames(dat$V) = ids

  # get data and break up
  yhat = dat$YPM ; U = dat$U ; V = dat$V ; rm(dat)

  # chose countries to label
  toLabel = trim(unlist(strsplit(cntryVec, ',')))

  # org data for circ plot
  if(paramsToPlot == 'U and V'){
    ggU = getDataForCirc(Y=yhat, U=U, V=V, vscale=.65,removeIsolates=FALSE)$uG }
  if(paramsToPlot == 'U'){
    ggU = getDataForCirc(Y=yhat, U=U, V=NULL, vscale=.65,removeIsolates=FALSE)$uG }
  if(paramsToPlot == 'V'){
    ggU = getDataForCirc(Y=yhat, U=NULL, V=V, vscale=.65,removeIsolates=FALSE)$uG }
  ggU = unique(ggU)
  ggU$ccols = cntryKey$ccols[match(ggU$actor,cntryKey$cowc)]
  ggU$lab = ggU$actor
  ggU$lab[!ggU$lab %in% toLabel] = ''
  ggU$lPch = ggU$tPch ; ggU$lPch[ggU$lab==''] = 0

  # viz
  circViz = ggplot(ggU, aes(x=X1, y=X2, size=tPch, color=actor)) +
      annotation_custom(mapForCirc, xmin=-.75, xmax=.75, ymin=-.75, ymax=.75) +
      geom_point(alpha=.65) + scale_size(range=c(4,8)) +
      ylab("") + xlab("") +
      geom_label_repel(aes(label=lab, size=lPch), max.overlaps = Inf) +
      scale_color_manual(values=ccols) +
      theme_light(base_family="Source Sans Pro") +
      theme(
          legend.position = 'none',
          panel.border = element_blank(), panel.grid=element_blank(),
          axis.ticks = element_blank(), axis.line=element_blank(),
          axis.text = element_blank() )
  #
  return(circViz)  }

getDistData = function(
  catSelect,
  configSelect,
  paramsToPlot,
  distToPlot,
  dyadVec
){
  if(catSelect=='Trade'){ dat=tradeMods }
  if(catSelect=='UN Voting'){ dat=unMods }

  # cntry pairs to calc dists for
  dyadsToViz = trim(unlist(strsplit(dyadVec, ',')))
  cntriesForViz = unique(trim(unlist(strsplit(dyadsToViz, '-'))))

  # subset to config and time
  modConfig = dat[[configSelect]]

  # check number of selected params
  numParams = (nchar(paramsToPlot)>2)+1

  # iterate through time
  distData = lapply(1:length(modConfig), function(tt){

    # iterate through parameters
    distDataParam = lapply(1:numParams, function(ii){

      # org params
      listParams = list()
      if(paramsToPlot=='U'){
        listParams[[1]] = modConfig[[tt]]$'U'
        names(listParams) = 'U'}
      if(paramsToPlot=='V'){
        listParams[[1]] = modConfig[[tt]]$'V'
        names(listParams) = 'V'}
      if(paramsToPlot=='U and V'){
        listParams[[1]] = modConfig[[tt]]$'U'
        listParams[[2]] = modConfig[[tt]]$'V'
        names(listParams) = c('U', 'V') }

      # subset to relev param
      paramMat = listParams[[ii]]

      # iterate through distance metrics
      distDataByMethod = lapply(distToPlot, function(distMethod){

        # relabel id attrs in mats
        ids=cntryKey$cowc[match(rownames(paramMat), cntryKey$cname)]
        rownames(paramMat) = ids

        # get distance calc
        paramMat = paramMat[cntriesForViz,]
        if(distMethod == 'cosine'){
          # calc cosine dist manually
          n = nrow(paramMat)
          distMat = matrix(0, n, n)
          for(i in 1:n){
            for(j in 1:n){
              if(i != j){
                dotprod = sum(paramMat[i,] * paramMat[j,])
                norm1 = sqrt(sum(paramMat[i,]^2))
                norm2 = sqrt(sum(paramMat[j,]^2))
                distMat[i,j] = 1 - (dotprod / (norm1 * norm2))
              }
            }
          }
          rownames(distMat) = colnames(distMat) = rownames(paramMat)
        } else {
          distMat = as.matrix(dist(paramMat, method=distMethod))
          rownames(distMat) = colnames(distMat) = rownames(paramMat)
        }

        # org
        out = reshape2::melt(distMat)
        out$year = num(names(modConfig)[tt])
        out$param = names(listParams)[ii]
        out$dist = distMethod
        out$lab = with(out, paste0( param, '_', dist ))
        return(out) })

      # org
      distDataByMethod = do.call('rbind', distDataByMethod)

      # close iteration through distance metrics
      return(distDataByMethod) })

    # org
    distDataParam = do.call('rbind', distDataParam)

    # close iteration through params
    return(distDataParam) })

  #
  distData = do.call('rbind', distData)

  # org by dyad pair
  distData$dyad = with(distData, paste0(Var1, '-', Var2))

  # subset to user chosen pairs
  # chose countries to label
  distData = distData[distData$dyad %in% dyadsToViz, ]

  #
  return(distData) }
##################################################################
