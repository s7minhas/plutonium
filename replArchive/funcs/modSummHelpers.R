coefp_colors = c(
  "Positive"=rgb(54, 144, 192, maxColorValue=255),
  "Negative"= rgb(222, 45, 38, maxColorValue=255),
  "Positive at 90"=rgb(158, 202, 225, maxColorValue=255),
  "Negative at 90"= rgb(252, 146, 114, maxColorValue=255),
  "Insig" = rgb(150, 150, 150, maxColorValue=255))

getConfInts = function(estData, beta='Estimate', serror='Std. Error'){
  estData$qt95hi = estData[,beta] + qnorm(.975)*estData[,serror]
  estData$qt95lo = estData[,beta] - qnorm(.975)*estData[,serror]
  estData$qt90hi = estData[,beta] + qnorm(.95)*estData[,serror]
  estData$qt90lo = estData[,beta] - qnorm(.95)*estData[,serror]
  return(estData) }

getSigLevs  = function(estData){
  estData$sig = NULL
  estData$sig[estData$qt90lo > 0 & estData$qt95lo < 0] = "Positive at 90"
  estData$sig[estData$qt95lo > 0] = "Positive"
  estData$sig[estData$qt90hi < 0 & estData$qt95hi > 0] = "Negative at 90"
  estData$sig[estData$qt95hi < 0] = "Negative"
  estData$sig[estData$qt90lo < 0 & estData$qt90hi > 0] = "Insig"
  return(estData) }

coefProcess = function(
  estData, betaLab='Estimate', serrorLab='Std. Error'
  ){
    return(
      getSigLevs(
        getConfInts(estData, betaLab, serrorLab)
        ) ) }

coefViz = function(ggData){
  viz = ggplot(
    data=ggData,
    aes(x=ivClean, y=Estimate, color=sig) ) +
    geom_point() +
    geom_linerange(aes(ymin=qt95lo, ymax=qt95hi), alpha = 1, size = 0.7) +
    geom_linerange(aes(ymin=qt90lo, ymax=qt90hi), alpha = 1, size = 1) +
    scale_color_manual(values=coefp_colors) +
    geom_hline(aes(yintercept=0), color='grey50', linetype='dashed') +
    labs(
      x='',
      y='',
      color=''
    ) +
    coord_flip() +
    facet_wrap(~modLab, nrow=1, scales='free_x') +
    theme_light(base_family="Source Sans Pro") +
    theme(
      legend.position='none',
      axis.ticks=element_blank(),
      panel.border=element_blank(),
      strip.text.x = element_text(
        size = 9, color='white',
        family="Source Sans Pro Semibold"),
    	strip.background = element_rect(fill = "#525252", color='#525252')
    )
  return(viz)
}
