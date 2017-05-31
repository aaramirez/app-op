
# Make sure a package is at least some version (only installs from CRAN)
ensure_version <- function(pkg, ver = "0.0") {
  if (system.file(package = pkg)  == "" || packageVersion(pkg) < ver)
    install.packages(pkg)
}

ensure_version("shiny", "1.0.0")
ensure_version("shinydashboard", "0.5.3")
ensure_version("fPortfolio", "3011.81")

library(shiny)
library(shinydashboard)
library(fPortfolio)

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll lower limit to 1MB.
options(shiny.maxRequestSize = 1*1024^2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  data <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.

    inFile <- input$file_w_data

    if (is.null(inFile))
      return(NULL)

    read.csv(inFile$datapath, header = input$header,
             sep = input$sep, quote = input$quote)
  })

  prices<- reactive({
    SMALLCAP[,data()$SYMBOLS]
  })

  returns<- reactive({
    SMALLCAP.RET[,data()$SYMBOLS]
  })

  output$datatable <- renderTable({
    data()
  })

  output$returntable<- renderTable({
    #TODO: Obtener los datos de la lista de símbolos
    returns()
  })

  output$symbollist<- renderUI({
    selectInput("symbol", "Seleccione el símbolo:",
                choices = data()$SYMBOLS
    )
  })

  output$priceplot<- renderPlot({
    indprices<-prices()[, input$symbol]
    plot(indprices)
  })

  output$returnplot<- renderPlot({
    indreturns<-returns()[, input$symbol]
    plot(indreturns)
  })

  output$meanvalue<- renderValueBox({
    valueBox(
      round(mean(prices()[, input$symbol]), digits = 4), "Media", icon = icon("sort-desc")
    )
  })

  output$varvalue<- renderValueBox({
    valueBox(
      round(stdev(prices()[, input$symbol])^2, digits = 4), "Varianza", icon = icon("line-chart"),
      color = "purple"
    )
  })

  output$stddevvalue<- renderValueBox({
    valueBox(
      round(stdev(prices()[, input$symbol]), digits = 4), "Desviación estandar", icon = icon("arrows-h"),
      color = "yellow"
    )
  })

})
