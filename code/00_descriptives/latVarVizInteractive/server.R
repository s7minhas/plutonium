#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

####
if(Sys.info()['user'] %in% c('S7M','s7m','herme','Owner')){
    u = Sys.info()['user']
    pth = paste0('C:/Users/', u, '/Research/plutonium/code/') }

if(Sys.info()['user'] %in% c('maxgallop')){
    u = Sys.info()['user']
    pth = paste0('/Users/', u, '/Documents/GitHub/plutonium/code/') }

if(Sys.info()['user'] %in% c('haeunchoi')){
    u = Sys.info()['user']
    pth = paste0('/Users/', u, '/Dropbox/myrepo/plutonium/code/') }


source(paste0(pth, 'setup.R'))

#
loadPkg(c(
    'shiny', 'ggplot2', 'plotly', 'ggthemes',
    'grid', 'png', 'philentropy' ))

#
source(paste0(pth, 'funcs/ameHelpers.R'))
####

####
load(file=paste0(pathOut, 'modsForApp.rda')) # tradeMods, unMods, icewsMods
mapForCirc = rasterGrob(readPNG(paste0(pathGraphics, 'mapLeg.png')), interpolate=TRUE)
load(paste0(pathGraphics, 'mapCol.rda'))
####

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    # choose config
    observe({
        # process user selection
        catSelect = input$catSelect
        if(input$catSelect=='Trade'){ catChoices=names(tradeMods) }
        if(input$catSelect=='UN Voting'){ catChoices=names(unMods) }
        if(input$catSelect=='ICEWS'){ catChoices=names(icewsMods) }

        # update choices for second select input
        updateSelectInput(session, "configSelect",
                          label="Choose model configuration: ",
                          choices = catChoices,
                          selected = catChoices[1]
                          )
    })

    # choose time slice
    observe({
        # process user selection
        configSelect = input$configSelect
        if(input$catSelect=='Trade'){ timeChoices=names(tradeMods[[configSelect]]) }
        if(input$catSelect=='UN Voting'){ timeChoices=names(unMods[[configSelect]]) }
        if(input$catSelect=='ICEWS'){ timeChoices=names(icewsMods[[configSelect]]) }

        # update choices for second select input
        if( any(is.null( timeChoices )) ){
            timeChoices = 2000:2020 }
        timeChoices = num(timeChoices)
        updateSliderInput(session, "timeSelect",
                          label="Choose time period: ",
                          min = min(timeChoices),
                          max = max(timeChoices),
                          value = min(timeChoices) )
    })

    # pull out model choice
    getData <- reactive({

        # config check
        if(input$configSelect=='Pick a category first.'){ return(NULL) }

        # subset to specific model
        if(input$catSelect=='Trade'){ dat=tradeMods }
        if(input$catSelect=='UN Voting'){ dat=unMods }
        if(input$catSelect=='ICEWS'){ dat=icewsMods }

        # subset to config and time
        dat = dat[[input$configSelect]][[char(input$timeSelect)]][c('YPM','U', 'V')]

        # modify labels
        ids=cntryKey$cowc[match(rownames(dat$YPM), cntryKey$cname)]
        rownames(dat$YPM) = colnames(dat$YPM) = rownames(dat$U) = rownames(dat$V) = ids

        #
        return(dat)
    })

    # visualize mult effs via circ
    output$circViz <- renderPlotly({

        # config check
        if(input$configSelect=='Pick a category first.'){ return(NULL) }

        # get data and break up
        dat = getData()
        yhat = dat$YPM ; U = dat$U ; V = dat$V ; rm(dat)

        # chose countries to label
        # toLabel = trim(unlist(strsplit(input$cntryVec, ',')))

        # org data for circ plot
        if(input$paramsToPlot == 'U and V'){
          ggU = getDataForCirc(Y=yhat, U=U, V=V, vscale=.65,removeIsolates=FALSE)$uG }
        if(input$paramsToPlot == 'U'){
          ggU = getDataForCirc(Y=yhat, U=U, V=NULL, vscale=.65,removeIsolates=FALSE)$uG }
        if(input$paramsToPlot == 'V'){
          ggU = getDataForCirc(Y=yhat, U=NULL, V=V, vscale=.65,removeIsolates=FALSE)$uG }
        ggU = unique(ggU)
        ggU$ccols = cntryKey$ccols[match(ggU$actor,cntryKey$cowc)]
        ggU$lab = ggU$actor
        # ggU$lab[!ggU$lab %in% toLabel] = ''
        ggU$lPch = ggU$tPch ; ggU$lPch[ggU$lab==''] = 0

        # viz
        circViz = ggplot(ggU, aes(x=X1, y=X2, size=tPch, color=actor)) +
            annotation_custom(mapForCirc, xmin=-.75, xmax=.75, ymin=-.75, ymax=.75) +
            geom_point(alpha=.6) + scale_size(range=c(4,8)) +
            ylab("") + xlab("") +
            # geom_label_repel(aes(label=lab, size=lPch), max.overlaps = Inf) +
            scale_color_manual(values=ccols) +
            theme_bw() +
            theme(
                legend.position = 'none',
                panel.border = element_blank(), panel.grid=element_blank(),
                axis.ticks = element_blank(), axis.line=element_blank(),
                axis.text = element_blank()
            )
        circViz = ggplotly(circViz, tooltip='actor')

        #
        return(circViz)
    })

    ## visualize distance in sender space
    output$distViz <- renderPlot({

        # config check
        if(input$configSelect=='Pick a category first.'){ return(NULL) }

        # subset to specific model
        if(input$catSelect=='Trade'){ dat=tradeMods }
        if(input$catSelect=='UN Voting'){ dat=unMods }
        if(input$catSelect=='ICEWS'){ dat=icewsMods }

        # subset to config and time
        modConfig = dat[[input$configSelect]]

        # check number of selected params
        numParams = (nchar(input$paramsToPlot)>2)+1

        # iterate through time
        distData = lapply(1:length(modConfig), function(tt){

          # iterate through parameters
          distDataParam = lapply(1:numParams, function(ii){

            # org params
            listParams = list()
            if(input$paramsToPlot=='U'){
              listParams[[1]] = modConfig[[tt]]$'U'
              names(listParams) = 'U'}
            if(input$paramsToPlot=='V'){
              listParams[[1]] = modConfig[[tt]]$'V'
              names(listParams) = 'V'}
            if(input$paramsToPlot=='U and V'){
              listParams[[1]] = modConfig[[tt]]$'U'
              listParams[[2]] = modConfig[[tt]]$'V'
              names(listParams) = c('U', 'V') }

            # subset to relev param
            paramMat = listParams[[ii]]

            # iterate through distance metrics
            distDataByMethod = lapply(input$distToPlot, function(distMethod){

              # relabel id attrs in mats
              ids=cntryKey$cowc[match(rownames(paramMat), cntryKey$cname)]
              rownames(paramMat) = ids

              # get distance calc
              distMat = distance(paramMat, method=distMethod, use.row.names=TRUE)

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
        dyadsToViz = trim(unlist(strsplit(input$dyadVec, ',')))
        distData = distData[distData$dyad %in% dyadsToViz, ]

        # viz
        ggDistViz = ggplot(
          distData, aes(x=year, y=value, color=dyad) ) +
          geom_point() +
          geom_line() +
          facet_grid(vars(dist), vars(param), scales='free') +
          labs(
            x='', y='', color=''
          ) +
          theme_bw() +
          theme(
            legend.position='bottom',
            panel.border=element_blank(),
            axis.ticks=element_blank(),
            axis.text.x=element_text(angle=45, hjust=1) )

      #
      return(ggDistViz)
    })


})
