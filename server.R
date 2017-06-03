
# Make sure a package is at least some version (only installs from CRAN)
ensure_version <- function(pkg, ver = "0.0") {
  if (system.file(package = pkg)  == "" || packageVersion(pkg) < ver)
    install.packages(pkg)
}

ensure_version("shiny", "1.0.0")
ensure_version("shinydashboard", "0.5.3")
ensure_version("fPortfolio", "3011.81")
ensure_version("knitr", "1.15.1")

library(shiny)
library(shinydashboard)
library(fPortfolio)
library(knitr)

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll lower limit to 1MB.
options(shiny.maxRequestSize = 1*1024^2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  lista<-c()
  data <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.

    inFile <- input$file_w_data

    if (is.null(inFile))
      return(NULL)
    as.timeSeries(read.csv(inFile$datapath, header = input$header,
             sep = input$sep, quote = input$quote))

  })

  prices<- function() {
    data()
  }

  returns<- function() {
    returnsresult<-rev(data())
    returnsresult<-diff(returnsresult)/returnsresult[,-length(returnsresult)]
    returnsresult[-1,]
  }

  symbols<- function() {
    names(data())
  }

  output$datatable <- renderTable({
    data()
  })

  output$returntable<- renderTable({
    #TODO: Obtener los datos de la lista de símbolos
    returns()
  })

  output$symbollist<- renderUI({
    selectInput("symbol", "Seleccione el símbolo:",
                choices = symbols()
    )
  })

  output$symbollist2<- renderUI({
    selectInput("symbol2", "",
                choices = symbols()
    )
  })

  output$symbollist3<- renderUI({
    selectInput("symbol3", "",
                choices = symbols()
    )
  })

  ## stats tab outputs - Begin
  covData<- reactive({
    covEstimator(returns())
  })

  output$meantable <- renderPrint({
    covData()$mu
  })

  output$varcovartable <- renderPrint({
    covData()$Sigma
  })

  ## stats tab outputs - End

  ## optimize tab outputs - Begin

  frontierCalc <- reactive({
    shortSpec<-portfolioSpec()
    setSolver(shortSpec)<-"solveRshortExact"
    portfolioFrontier(returns(), spec=shortSpec,constraints="Short")
  })

  output$efplot <- renderPlot({
    #print(shortFrontier)
    frontierPlot(frontierCalc(), frontier = "both", risk="Sigma", type="l")
    minvariancePoints(frontierCalc(), pch=19, col="red")
    singleAssetPoints(frontierCalc(), risk = "Sigma", pch=19, cex=1.5, col=topo.colors(6))
  })

  output$wplot <- renderPlot({
    weightsPlot(frontierCalc())
  })

  output$vmtext <- renderPrint({
    minvariancePortfolio(returns())
    Spec = portfolioSpec()
    setSolver(Spec)<-"solveRshortExact"
    setTargetReturn(Spec) = input$mvmu
    efficientPortfolio(returns(), Spec)
  })

  ## optimize tab outputs - End


  ## individual tab outputs - Begin

  output$priceplot<- renderPlot({
    plot(prices()[, input$symbol])
  })

  output$returnplot<- renderPlot({
    plot(returns()[, input$symbol])
  })

  output$meanvalue<- renderValueBox({
    valueBox(
      round(mean(returns()[, input$symbol]), digits = 4), "Media", icon = icon("balance-scale")
    )
  })

  output$varvalue<- renderValueBox({
    valueBox(
      round(stdev(returns()[, input$symbol])^2, digits = 4), "Varianza", icon = icon("line-chart"),
      color = "purple"
    )
  })

  output$stddevvalue<- renderValueBox({
    valueBox(
      round(stdev(returns()[, input$symbol]), digits = 4), "Desviación estandar", icon = icon("arrows-h"),
      color = "yellow"
    )
  })

  ## individual tab outputs - End

  ## pairs tab outputs - Begin

  output$covarvalue<- renderValueBox({
    valueBox(
      round(cov(returns()[, input$symbol2],returns()[, input$symbol3]), digits = 4),
      "Covarianza", icon = icon("line-chart"),
      color = "purple"
    )
  })

  output$correlvalue<- renderValueBox({
    valueBox(
      round(cor(returns()[, input$symbol2],returns()[, input$symbol3]), digits = 4),
      "Correlación", icon = icon("arrows-h"),
      color = "yellow"
    )
  })

  output$pricesplot<- renderPlot({
    plot(prices()[, input$symbol2])
  })

  output$returnsplot<- renderPlot({
    plot(returns()[, input$symbol2])
  })

  output$rsplot<- renderPlot({
    plot(prices()[, input$symbol2]/prices()[, input$symbol3])
  })

  ## pairs tab outputs - End

})
