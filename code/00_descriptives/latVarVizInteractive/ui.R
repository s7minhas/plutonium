#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
####

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Liberal Order Measurement Blah Blah"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("catSelect",
                        "Input Category:",
                        choices=c('Trade', 'UN Voting', 'ICEWS'),
                        selected='Trade'
                        ),
            selectInput('configSelect',
                        label="Choose model configuration: ",
                        choices = 'Pick a category first.',
                        selected=''
                        ),
            selectInput('paramsToPlot',
                        label="Choose model parameter to plot: ",
                        choices = c('U', 'V', 'U and V'),
                        selected='U and V'
                        ),
            sliderInput('timeSelect',
                        label="Choose year: ",
                        min = 2000, max=2020, value=2000,
                        sep='', animate=TRUE),
            textInput('cntryVec',
                      'Enter comma delimited set of countries in cowc format:',
                      "USA, CHN"
                      )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("circViz")
        )
    )
))
