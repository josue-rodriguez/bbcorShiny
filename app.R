#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinyBS)
# library(periscope)
library(BBcor)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("flatly"),

    # 1.0) Application title
    titlePanel(("BBcor: Bayesian Bootsrapped Correlations")),

    # 2.0) Begin sidebar layout for app 
    sidebarLayout(
        # 2.1) Begin side panel with install instructions and input options
        sidebarPanel(
            # h3(strong("Installation")), # 2.1.1) Installation instructions
            # p("BBcor is available on CRAN, so you can install it in the usual way from your R console:"),
            # p(code("install.packages('BBcor')")), 
            # p("Or from GitHub"),
            # p(code("remotes::install_github('donaldRwilliams/BBcor'")),
            
            # br(),
            
            p(h3(strong("Arguments"))), # 2.1.2) Input options for BBcor
            
            fileInput("dataFile", 
                      label = "Upload Data",
                      accept = c(".csv", ".rds")) ,
            helpText("To use this app, please upload either a .csv or .rds file containing your data"),
           
             selectInput("variableSubset",
                         label = "Choose variables to correlate",
                         choices = NULL,
                         selected = NULL,
                         multiple = TRUE),
            helpText("Please select at least 2 columns to preview data. If none are selected, the entire dataset is used."),
            checkboxInput("plotPairs", label = "Pairs Plot"),
            
            selectInput("corrType",
                        label = "Correlation Method",
                        choices = c("Pearson" = "pearson", 
                                    "Spearman" = "spearman", 
                                    "Kendall" = "kendall",
                                    "Gaussian Rank" = "gaussian_rank",
                                    "Polychoric" = "polychoric",
                                    "Blomqvist" = "blomqvist")),
            helpText("Note: Polychoric correlations require all variables to have 8 or less categories and may take a while to finish sampling."),
            bsTooltip("corrType", 
                      "Select Correlation Type", 
                      placement = "right",
                      trigger = "focus",
                      options = list(container = "body")),
            
            numericInput("iter",
                         label = "Iterations",
                         value = 1000,
                         min = 0,
                         max = 10000),
            bsTooltip("iter",
                      "Number of samples to collect",
                      placement = "right",
                      trigger = "focus",
                      options = list(container = "body")),
            
            numericInput("cores",
                         label = "Cores",
                         value = 1,
                         min = 1,
                         max = 8),
            bsTooltip("cores",
                      "Number of cores to use if parallel computing is desired",
                      placement = "right",
                      trigger = "focus",
                      options = list(container = "body")),
            
            numericInput("ci",
                         label = "Credible Interval",
                         value = 0.90,
                         min = 0, 
                         max = 1),
            bsTooltip("ci",
                      "Desired length of credible interval",
                      placement = "right",
                      trigger = "focus",
                      options = list(container = "body")),
            
            
            actionButton("runBBCorr", label = "Get Correlations")
            
            
        ),
        # 2.2) Main panel for output
        mainPanel(
          tabsetPanel(
            tabPanel("Data Preview",
                     fluid = TRUE,
                     dataTableOutput("dataView"),
                     plotOutput("dataPairsPlot")),
            tabPanel("BBcor Output",
                     fluid = TRUE,
                     uiOutput("summaryHeader"),
                     verbatimTextOutput("bbCorrResults"),
                     plotOutput("bbCorrPlot"),
                     downloadButton("downloadBBPlot", 
                                    label = "Download Plot"),
                     bsTooltip("downloadBBPlot",
                               "Download plot after running 'Get Correlations'",
                               placement = "top",
                               trigger = "hover",
                               options = list(container = "body")))
          )
        )
        
      )
)

# Define server logic
server <- function(input, output, session) {
    observe({
        updateSelectizeInput(session, "variableSubset", choices = colnames(getDataFrame()))
    })
    # 1.0) Prepare data
    # 1.1) Function to read in data
    getDataFrame <- reactive({
        inFile <- input$dataFile
        
        if(is.null(inFile)) {
            return(NULL)
        }
        
        path <- inFile$datapath
        
        if (grepl("\\.csv$", path)) {
            tbl <- read.csv(path)
            return(tbl)
        } else {
            tbl <- readRDS(path)
            return(tbl)
        }
    })
    
    # 1.2) View data
    output$dataView <- renderDataTable({
      dataFrame <- getDataFrame() 
      if (is.null(input$variableSubset)) {
          return(dataFrame) 
      } else {
        return(dataFrame[, input$variableSubset])  
      }
    },
    options = list(
      pageLength = 5,
      lengthMenu = c(5, 10, 15, 20)
    ))
    
    # 1.3) Pairs plot
    output$dataPairsPlot <- renderPlot({
      if (input$plotPairs) {
        dataFrame <- getDataFrame()
        if (is.null(dataFrame)) {
          return(NULL)
        } else if (!is.null(input$variableSubset)) {
          dataFrame <- dataFrame[, input$variableSubset]
        } 
        GGally::ggpairs(dataFrame) + theme_bw()
      } else {
        return(NULL)
      }
    })
    # 2.0) run BB and print output
    observeEvent(input$runBBCorr, 
                 {
                     if (is.null(input$variableSubset)) {
                         dataFrame <- getDataFrame()
                     } else {
                         dataFrameRaw <- getDataFrame()
                         dataFrame <- dataFrameRaw[, input$variableSubset]
                     }

                     bb <- bbcor(dataFrame,
                                 method = input$corrType,
                                 iter = input$iter,
                                 cores = input$cores)
                     summ <- summary(bb, ci = input$ci)
                     
                     output$summaryHeader <- renderUI({h1("Posterior Summary")})
                     output$bbCorrResults <- renderPrint({print(summ)})
                     output$bbCorrPlot <- renderPlot({plot(bb) + theme_bw()})
                     output$downloadBBPlot <- downloadHandler(filename = "plot_download.jpg",
                                                              content = function(file) {
                                                                ggsave(file, device = "jpeg")
                                                                }
                                                              )
                 })
}


# plotInput = function() {
#   if (input$plotPairs) {
#     dataFrame <- getDataFrame()
#     if (is.null(dataFrame)) {
#       return(NULL)
#     } else if (!is.null(input$variableSubset)) {
#       dataFrame <- dataFrame[, input$variableSubset]
#     } 
#     GGally::ggpairs(dataFrame) + theme_bw()
# }
# callModule(downloadablePlot,
#            "object_id1", 
#            filenameroot = "mydownload1",
#            aspectratio = 1.33,
#            downloadfxns = list(png = plotInput),
#            visibleplot = plotInput)
# 
# }

# Run the application 
shinyApp(ui = ui, server = server)
