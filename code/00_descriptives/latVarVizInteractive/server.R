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

if(Sys.info()['user'] %in% c('haeunchoi')){
    u = Sys.info()['user']
    pth = paste0('C:/Users/', u, '/Dropbox/myrepo/plutonium/code/') }


source(paste0(pth, 'setup.R'))

#
loadPkg(c(
    'shiny', 'ggplot2',
    'grid', 'png' ))

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
    
    # visualize
    output$circViz <- renderPlot({

        # config check
        if(input$configSelect=='Pick a category first.'){ return(NULL) }        
                
        # get data and break up
        dat = getData()
        yhat = dat$YPM ; U = dat$U ; V = dat$V ; rm(dat)
        
        # chose countries to label
        toLabel = trim(unlist(strsplit(input$cntryVec, ',')))
        
        # org data for circ plot
        ggU = getDataForCirc(Y=yhat, U=U, V=V, vscale=.65,removeIsolates=FALSE)$uG
        ggU = unique(ggU)
        ggU$ccols = cntryKey$ccols[match(ggU$actor,cntryKey$cowc)]
        ggU$lab = ggU$actor
        ggU$lab[!ggU$lab %in% toLabel] = ''
        ggU$lPch = ggU$tPch ; ggU$lPch[ggU$lab==''] = 0
        
        # viz
        circViz = ggplot(ggU, aes(x=X1, y=X2, size=tPch, color=actor)) +
            annotation_custom(mapForCirc, xmin=-.75, xmax=.75, ymin=-.75, ymax=.75) +
            geom_point(alpha=.9) + scale_size(range=c(4,8)) +
            ylab("") + xlab("") +
            geom_label_repel(aes(label=lab, size=lPch), max.overlaps = Inf) +
            scale_color_manual(values=ccols) +
            theme_bw() +
            theme(
                legend.position = 'none',
                panel.border = element_blank(), panel.grid=element_blank(),
                axis.ticks = element_blank(), axis.line=element_blank(),
                axis.text = element_blank()
            )
        
        #
        return(circViz)
    })
})
