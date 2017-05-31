library(shiny)
library(shinythemes)

shinyUI(pageWithSidebar(
  
  headerPanel("Data Exploration"),
  sidebarPanel(
    
    conditionalPanel(condition="input.tabselected==1",
                     fileInput('file1', 'Choose CSV File',
                               accept=c('text/csv', 
                                        'text/comma-separated-values,text/plain', 
                                        '.csv')),
                     tags$hr(),
                     checkboxInput('header', 'Header', TRUE),
                     radioButtons('sep', 'Separator',
                                  c(Comma=',',
                                    Semicolon=';',
                                    Tab='\t'),
                                  ','),
                     radioButtons('quote', 'Quote',
                                  c(None='',
                                    'Double Quote'='"',
                                    'Single Quote'="'"),
                                  '"')),
    conditionalPanel(condition="input.tabselected==2",
                     selectInput("dataset", "Select Data Set to Explore", choices=c(ls('package:datasets'),'MyTable'), 
                                 selected = "mtcars"),
                     radioButtons("choice","Choose an Option", choices=c("Dataset" = 1, "Structure" = 2,
                                                                         "Summary" = 3, "Correlation" = 4))
                     
    ),
    
    conditionalPanel(condition="input.tabselected==3",uiOutput("feat1"),uiOutput("feat2"),uiOutput("outc"))
    
  ),
  mainPanel(

    tabsetPanel(
      tabPanel("Main", value=1, tableOutput('MyTable'),actionButton(inputId = "reload", label = "Reload data")),
      tabPanel("Data", value=2, conditionalPanel(condition="input.choice==1", verbatimTextOutput("dat")),
               conditionalPanel(condition="input.choice==2", verbatimTextOutput("struct")),
               conditionalPanel(condition="input.choice==3", verbatimTextOutput("summary")),
               conditionalPanel(condition="input.choice==4", verbatimTextOutput("correlation"))),
      tabPanel("Plot", value=3, plotOutput("plot")), 
      id = "tabselected"
      )
    )
  ))