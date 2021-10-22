#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

pth = paste0(here::here(), '/')
source(paste0(pth, 'setup.R'))

#
loadPkg(c(
    'reshape2', 'amen', 'tidyr',
    'ggplot2', 'tidyverse',
    'cshapes', 'countrycode' ))
####

####
load(file=paste0(pathOut, 'tradeMods.rda')) # tradeMods
load(file=paste0(pathOut, 'unMods.rda')) # unMods
load(file=paste0(pathOut, 'icewsMods.rda')) # icewsMods
####

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
))
