#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(lubridate)
library(zoo)
library(PerformanceAnalytics)
library(ggplot2)
library(knitr)
library(kableExtra)
library(gridExtra)
library(RColorBrewer)
library(plotly)
source("pgp_utilities.r")
pgp <- readRDS("data/pgp.rds")
choiceNamesPortfolios <- function(stockwts){
    out <- paste0(100*stockwts, "% Stock / ", 100-100*stockwts, "% Bond")
    out[stockwts == 0] <- "100% Bond"
    out[stockwts == 1] <- "100% Stock"
    return(out)
}

eom <- function(dt){
    rollback(ceiling_date(dt, unit="month"))
}


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("PGP Hypothetical Performance"),
 
   sidebarLayout(
      sidebarPanel(width=2, 
          conditionalPanel(condition = 
              "input.selectedtab == 1 || 
               input.selectedtab == 2 ||
               input.selectedtab == 3 ||
               input.selectedtab == 5 ||
               input.selectedtab == 7",
              checkboxGroupInput("cbPGP", "PGP / Bond Weights",
                        choiceNames = paste0(100*pgp$params$pgpwts, "% PGP / ", 100-100*pgp$params$pgpwts, "% Bond"),
                        choiceValues = sapply(pgp$params$pgpwts, get_pgp_name),
                        selected = sapply(pgp$params$pgpwts, get_pgp_name)[1]
                        ),
              checkboxGroupInput("cbPortfolios", "Benchmark Portfolios",
                        choiceNames = c("100% Cash", choiceNamesPortfolios(pgp$params$stockwts)),
                        choiceValues = c("Cash", sapply(pgp$params$stockwts, get_port_name)),
                        selected = c("Cash", sapply(pgp$params$stockwts, get_port_name)[c(7,11)])
                        )
                        ),
          conditionalPanel(condition = 
                            "input.selectedtab == 6 ||
                            input.selectedtab == 4 ||
                            input.selectedtab == 8 ||
                            input.selectedtab == 9",
            radioButtons("rbPGP", "PGP / Bond Weights",
                         choiceNames = paste0(100*pgp$params$pgpwts, "% PGP / ", 100-100*pgp$params$pgpwts, "% Bond"),
                         choiceValues = sapply(pgp$params$pgpwts, get_pgp_name)
                         ),
            radioButtons("rbPortfolios", "Benchmark Portfolios",
                         choiceNames = c("100% Cash", choiceNamesPortfolios(pgp$params$stockwts)),
                         choiceValues = c("Cash", sapply(pgp$params$stockwts, get_port_name)),
                         selected = c("Cash", sapply(pgp$params$stockwts, get_port_name))[8]
                         )
            # Start with decade view instead of default month view
                         ),
          sliderInput("daterange1", "Date range slider:",
                      min = pgp$g1d$Date[1],
                      step=30,
                      max = last(pgp$g1d$Date),
                      timeFormat = "%b-%Y",
                      value = c(as.Date("1945-12-31"),last(pgp$g1d$Date)))  # pgp$g1d$Date[1]
         
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
          tabsetPanel(id = "selectedtab",
                      tabPanel("Returns Table", value=1, 
                               tableOutput("growthTable")
                               ),
                      tabPanel("Growth of $1", value=2, 
                               plotlyOutput("g1d2"),
                               h5(textOutput("txtg1d", container = span))),
                      tabPanel("Box Plots", value=3, plotlyOutput("boxplots")),
                      tabPanel("Rolling Returns", value=4, plotlyOutput("rollingplots")),
                      tabPanel("Market Drawdowns", value=5, tableOutput("mktddtable")),
                      tabPanel("Worst Drawdowns", value=6, 
                               h5(textOutput("txtpgpddtable", container = span)),
                               tableOutput("pgpddtable"),
                               h5(textOutput("txtportddtable", container = span)),
                               tableOutput("portddtable")),
                      tabPanel("Cal. Returns", value=7,
                               tableOutput("calyrtable")),
                      tabPanel("ScatterPlots", value=8,
                               uiOutput("scatterplots")),
                      tabPanel("Capture", value=9,
                               uiOutput("plots"))
                               ),
          verbatimTextOutput("textrbpgp"),
          verbatimTextOutput("textrbport"),
          verbatimTextOutput("text1"),
         verbatimTextOutput("text2"),
         verbatimTextOutput("dateRangeText"),
         tableOutput("datestart")
         
         
      )
   )
)

server <- function(input, output) {
    dr_in <- reactive({
        data.frame(
            Name = c("start", "end"),
            Value = as.character(eom(input$daterange1)),
            stringsAsFactors = FALSE
        )
        })
    #date_range <- paste0(as.character(eom(input$daterange1)), collapse = "/")
    output$textrbpgp <- renderText(input$rbPGP)
    output$textrbport <- renderText(input$rbPortfolios)
    output$text1 <- renderText(input$cbPGP)
    output$text2 <- renderText(input$cbPortfolios)
    output$datestart <- renderTable(dr_in())
    output$dateRangeText  <- renderText(
         paste0(as.character(eom(input$daterange1)), collapse = "/"))
    #output$g1d <- renderPlot({plot.pgp(pgp)})
    #output$g1d2 <- renderPlotly({create_g1d_plotly2(pgp)})
    output$g1d2 <- renderPlotly({plotly_g1d(pgp, 
                                            paste0(as.character(eom(input$daterange1)), collapse = "/"), 
                                            input$cbPGP,
                                            input$cbPortfolios)})
    output$growthTable <- renderTable(make_table_growth(pgp, 
                                      paste0(as.character(eom(input$daterange1)), collapse = "/"), 
                                      input$cbPGP,
                                      input$cbPortfolios),
                                      striped = TRUE,
                                      hover = TRUE,
                                      rownames = TRUE,
                                      spacing = "s", 
                                      digits = 1)
    output$boxplots <- renderPlotly({
        create_boxplot_grid_plotly(pgp, 
                                   paste0(as.character(eom(input$daterange1)), collapse = "/"), 
                                   input$cbPGP,
                                   input$cbPortfolios)}) 
    output$rollingplots <- renderPlotly({
        create_rolling_grid_plotly(pgp, 
                                   paste0(as.character(eom(input$daterange1)), collapse = "/"), 
                                   input$rbPGP,
                                   input$rbPortfolios)})
    output$mktddtable <- renderTable(create_mkt_drawdown_table(pgp, 
                                     paste0(as.character(eom(input$daterange1)), collapse = "/"), 
                                     input$cbPGP, 
                                     input$cbPortfolios),
                                     striped=TRUE, hover=TRUE, spacing = "s", digits=1)
    output$pgpddtable <- renderTable(create_drawdown_table(pgp, 
                                     paste0(as.character(eom(input$daterange1)), collapse = "/"), 
                                     input$rbPGP),
                                     striped=TRUE, hover=TRUE, spacing = "s", digits=1)
    output$portddtable <- renderTable(create_drawdown_table(pgp=pgp, 
                                                           daterange=paste0(as.character(eom(input$daterange1)), collapse = "/"), 
                                                           varname=input$rbPortfolios),
                                     striped=TRUE, hover=TRUE, spacing = "s", digits = 1)
    output$txtportddtable <- renderText(paste(input$rbPortfolios, "drawdowns of at least 10%"))
    output$txtpgpddtable <- renderText(paste(input$rbPGP, "drawdowns of at least 10%"))
    # output$txtg1d <- renderText(paste("Pink shaded areas represent periods invested in stocks (", 
    #                                   round(percent_risky(pgp, paste0(as.character(eom(input$daterange1)), collapse = "/")),1),
    #                                   ");",
    #                                   "unshaded areas represent periods in cash(", 
    #                                   100 - round(percent_risky(pgp, paste0(as.character(eom(input$daterange1)), collapse = "/")),1),
    #                                   ")"))
    output$txtg1d <- renderText(paste0("Pink areas represent periods invested in stocks (",
                                      round(percent_risky(pgp, paste0(as.character(eom(input$daterange1)), collapse = "/")),1),
                                      "%); unshaded areas represent periods in cash (",
                                      round(100-percent_risky(pgp, paste0(as.character(eom(input$daterange1)), collapse = "/")),1),
                                      "%)."
                                      ))
    output$calyrtable <- renderTable(create_calyr_table(pgp, 
                                                               paste0(as.character(eom(input$daterange1)), collapse = "/"), 
                                                               input$cbPGP, 
                                                               input$cbPortfolios),
                                     striped=TRUE, hover=TRUE, spacing = "s", digits=1, rownames = TRUE)
    
    nrolling <- length(pgp$params$rolling)
    output$plots <- renderUI({
        plot_output_list <- lapply(1:nrolling, function(i) {
            plotname <- paste("plot", i, sep="")
            plotlyOutput(plotname)
        })
        
        # Convert the list to a tagList - this is necessary for the list of items
        # to display properly.
        do.call(tagList, plot_output_list)
    })
    output$scatterplots <- renderUI({
        scatterplot_output_list <- lapply(1:nrolling, function(i) {
            scatterplotname <- paste("scatterplot", i, sep="")
            plotlyOutput(scatterplotname)
        })
        
        # Convert the list to a tagList - this is necessary for the list of items
        # to display properly.
        do.call(tagList, scatterplot_output_list)
    })
    
    # Call renderPlot for each one. Plots are only actually generated when they
    # are visible on the web page.
    for (i in 1:nrolling) {
        # Need local so that each item gets its own number. Without it, the value
        # of i in the renderPlot() will be the same across all instances, because
        # of when the expression is evaluated.
        local({
            my_i <- i
            plotname <- paste("plot", my_i, sep="")
            
            output[[plotname]] <- renderPlotly({
                create_1capture_plotly(pgp,
                                       rev(pgp$params$rolling)[my_i],
                                       paste0(as.character(eom(input$daterange1)), collapse = "/"), 
                                       input$rbPGP,
                                       input$rbPortfolios)
            })
            
            scatterplotname <- paste("scatterplot", my_i, sep="")
            
            output[[scatterplotname]] <- renderPlotly({
                create_scatter_pair_plotly(pgp,
                                       rev(pgp$params$rolling)[my_i],
                                       paste0(as.character(eom(input$daterange1)), collapse = "/"), 
                                       input$rbPGP,
                                       input$rbPortfolios)
            })
        })
    }
       
}

# Run the application 
shinyApp(ui = ui, server = server)

