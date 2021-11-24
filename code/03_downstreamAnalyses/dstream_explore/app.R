#########
library(shiny)
library(kableExtra)
#########

#########
if(Sys.info()['user'] %in% c('S7M','s7m','herme','Owner')){
    suf = paste0('C:/Users/', Sys.info()['user'], '/')
    pathDrop = paste0(suf, 'Dropbox/Research/plutonium/')
    pathIn = paste0(pathDrop, 'data/')
    pathOut = paste0(pathDrop, 'results/')
    pathGraphics = paste0(pathDrop, 'graphics/') }

if(Sys.info()['user'] %in% c('maxgallop')){
    suf = paste0('/Users/', Sys.info()['user'], '/')
    pathDrop = paste0(suf, 'Dropbox/plutonium/')
    pathIn = paste0(pathDrop, 'data/')
    pathOut = paste0(pathDrop, 'results/')
    pathGraphics = paste0(pathDrop, 'graphics/') }


if(Sys.info()['user'] %in% c('pjb15180')){
  suf = paste0('C:/Users/', Sys.info()['user'], '/')
  pathDrop = paste0(suf, 'Dropbox/plutonium/')
  pathIn = paste0(pathDrop, 'data/')
  pathOut = paste0(pathDrop, 'results/')
  pathGraphics = paste0(pathDrop, 'graphics/') }



if(Sys.info()['user'] %in% c('haeunchoi')){
    suf = paste0('/Users/', Sys.info()['user'], '/')
    pathDrop = paste0(suf, 'Dropbox/plutonium/')
    pathIn = paste0(pathDrop, 'data/')
    pathOut = paste0(pathDrop, 'results/')
    pathGraphics = paste0(pathDrop, 'graphics/') }
#########

#########
# load data
load(paste0(pathOut, 'lmerMods.rda'))
load(paste0(pathOut, 'modelInfo.rda'))
#########

#########
# Define UI
ui <- fluidPage(

    # 
    titlePanel("dstream results"),

    # 
    sidebarLayout(
        sidebarPanel(
            selectInput("dv",
                        "dependent variable:",
                        choices=unique(modsToRun$dv),
                        selected=unique(modsToRun$dv)[1]),
            selectInput("ivs",
                        "indep vars:",
                        choices=unique(modsToRun$ivs),
                        selected=unique(modsToRun$ivs)[1]),
            selectInput("lagDV",
                        "include lagged dv:",
                        choices=unique(modsToRun$lagDV),
                        selected=FALSE),
            selectInput("re",
                        "rand eff:",
                        choices=unique(modsToRun$re),
                        selected=unique(modsToRun$re)[1]),
            selectInput("type",
                        "rand eff struct:",
                        choices=unique(modsToRun$type),
                        selected=unique(modsToRun$type)[1])
        ),

        # 
        mainPanel(
           tableOutput("coefTab")
        )
    )
)
#########

#########
# Define server logic
server <- function(input, output) {

    output$coefTab <- function(){
        
        #
        req(input)
        
        # subset to relevant model
        toPick = which(
            modsToRun$dv==input$dv & 
            modsToRun$ivs==input$ivs &
            modsToRun$re==input$re &
            modsToRun$type==input$type &
            modsToRun$lagDV==input$lagDV
            )
        
        # 
        mod = lmerMods[[toPick]]$effects    
        
            
        # 
        kable_styling(kbl(mod, digits=2), 'hover')
    }
}
#########

#########
# Run the application 
shinyApp(ui = ui, server = server)
#########