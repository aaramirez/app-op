
# Make sure a package is at least some version (only installs from CRAN)
ensure_version <- function(pkg, ver = "0.0") {
  if (system.file(package = pkg)  == "" || packageVersion(pkg) < ver)
    install.packages(pkg)
}

ensure_version("shiny", "1.0.0")
ensure_version("shinydashboard", "0.5.3")
ensure_version("fPortfolio", "3011.81")
ensure_version("knitr", "1.15.1")
ensure_version("timeSeries", "3022.101.2")
ensure_version("pastecs", "1.3-18")

library(shiny)
library(shinydashboard)
library(timeSeries)
library(knitr)
library(fPortfolio)
library(pastecs)


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
    read.csv(inFile$datapath, header = input$header,
             sep = input$sep, quote = input$quote)

  })

  prices<- function() {
    as.timeSeries(data())
  }

  method<- reactive({
    if (input$returnsType == "arithmetic") {
      "discrete"
    } else {
      "continuous"
    }
  })

  returns<- function() {
    #returnsresult<-exp(diff(log(prices()))) - 1
    ##returnsresult<-diff(log(prices()))
    timeSeries::returns(prices(), method=method())
  }

  symbols<- function() {
    names(prices())
  }

  cumulated<- function() {
    timeSeries::cumulated(returns(), method=method())
  }

  drawdowns<- function() {
    timeSeries::drawdowns(returns())
  }

  smoothlowess<- reactive({
    timeSeries::smoothLowess(prices()[, input$symbol])
  })

  turnpoints<- function() {
    x<-smoothlowess()[,"lowess"]
    tp<-suppressWarnings(pastecs::turnpoints(as.ts(x)))
    recordIDs<- data.frame(tp$peaks, tp$pits)
    rownames(recordIDs)<- rownames(x)
    colnames(recordIDs)<- c("peaks", "pits")
    timeSeries(data=x, charvec=time(x),
               units = colnames(x),
               zone = finCenter(x),
               FinCenter = finCenter(x),
               recordIDs = recordIDs,
               title = x@title,
               documentation = x@documentation
               )
  }


  # data tab outputs - Begin

  output$datatable <- renderTable({
    round(prices(), digits = 6)
  })

  output$returntable<- renderTable({
    round(returns(), digits = 6)
  })

  output$cumulatedtable <- renderTable({
    cumulated()
  })

  output$drawdowstable <- renderTable({
    drawdowns()
  })

  # data tab outputs - End

  ## stats tab outputs - Begin

  covData<- reactive({
    covEstimator(returns())
  })

  output$summarypricestext<- renderPrint({
    summary(prices())
  })

  output$summaryreturnstext<- renderPrint({
    summary(returns())
  })

  output$basicstatspricestext<- renderPrint({
    fBasics::basicStats(prices())
  })

  output$basicstatsreturnstext<- renderPrint({
    fBasics::basicStats(returns())
  })

  output$returnsquantilestext<- renderPrint({
    stats::quantile(returns(), probs=seq(0, 1, 0.05), type=as.integer(input$quantiletype))
  })

  output$returnscolquantilestext<- renderPrint({
    timeSeries::colQuantiles(returns(),
                             prob=as.double(input$quantileprob),
                             type=as.integer(input$quantiletype2))
  })

  output$meantable <- renderPrint({
    covData()$mu
  })

  output$varcovartable <- renderPrint({
    covData()$Sigma
  })

  output$symbollist4<- renderUI({
    selectInput("symbol4", "",
                choices = symbols()
    )
  })

  output$drawdownstable<- renderTable({
    drawdownsStats(returns()[,input$symbol4])
  })

  output$outlierstext<- renderPrint({
    fAssets::assetsOutliers(returns(),
                            timeSeries::colMeans(returns()),
                            stats::cov(returns()))
  })

  amcov<- function() {
    fAssets::assetsMeanCov(returns()*100, method = input$meancovmethod)
  }

  output$meancovtext<- renderPrint({
    amcov()
  })

  output$covellipsesplot<- renderPlot({
    fAssets::covEllipsesPlot((list(cov(returns()), amcov()$cov)))
    title(main = paste0("Sample vs. ", input$meancovmethod," Covariances"))
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

  output$symbollist<- renderUI({
    selectInput("symbol", "Seleccione el símbolo:",
                choices = symbols()
    )
  })

  output$meanvalue<- renderValueBox({
    valueBox(
      round(mean(returns()[, input$symbol]), digits = 6), "Media", icon = icon("balance-scale")
    )
  })

  output$varvalue<- renderValueBox({
    valueBox(
      round(stdev(returns()[, input$symbol])^2, digits = 6), "Varianza", icon = icon("line-chart"),
      color = "purple"
    )
  })

  output$stddevvalue<- renderValueBox({
    valueBox(
      round(stdev(returns()[, input$symbol]), digits = 6), "Desviación estandar", icon = icon("arrows-h"),
      color = "yellow"
    )
  })

  output$priceplot<- renderPlot({
    seriesPlot(prices()[, input$symbol])
  })

  output$returnplot<- renderPlot({
    seriesPlot(returns()[, input$symbol])
  })

  output$returnshistplot<- renderPlot({
    rdata<-returns()[, input$symbol]
    h<-hist(rdata, freq = FALSE, main = "", xlab = "returns")
    #xfit<-seq(min(rdata),max(rdata),length=100)
    #yfit<-dnorm(xfit,mean=mean(rdata),sd=sd(rdata))
    #yfit <- yfit*diff(h$mids[1:2])*length(rdata)
    #lines(xfit, yfit, col="blue", lwd=2)
  })

  output$returnsdensityplot<- renderPlot({
    plot(density(returns()[, input$symbol]), main="")
  })

  output$lowessplot <- renderPlot({
    plot(smoothlowess()[,input$symbol])
    lines(smoothlowess()[,"lowess"], col="blue")
  })

  output$turnsplot <- renderPlot({
    turnsvar<-turnpoints()
    peaks<-turnsvar[turnsvar@recordIDs[,"peaks"]==TRUE,]
    pits<-turnsvar[turnsvar@recordIDs[,"pits"]==TRUE,]
    seriesPlot(smoothlowess()[,input$symbol])
    lines(smoothlowess()[,"lowess"], col="blue", lwd=2)
    points(peaks, col="green3", pch=24)
    points(pits, col="red", pch=25)
  })

  ## individual tab outputs - End

  ## pairs tab outputs - Begin

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

  output$covarvalue<- renderValueBox({
    valueBox(
      round(cov(returns()[, input$symbol2],returns()[, input$symbol3]), digits = 6),
      "Covarianza", icon = icon("line-chart"),
      color = "purple"
    )
  })

  output$correlvalue<- renderValueBox({
    valueBox(
      round(cor(returns()[, input$symbol2],returns()[, input$symbol3]), digits = 6),
      "Correlación", icon = icon("arrows-h"),
      color = "yellow"
    )
  })

  output$pricesplot<- renderPlot({
    seriesPlot( prices()[, input$symbol2],
          ylim=c(min(min(prices()[, input$symbol2]),
                     min(prices()[, input$symbol3])),
                 max(max(prices()[, input$symbol2]),
                     max(prices()[, input$symbol3]))))
    lines(prices()[, input$symbol3],
          col="blue")
  })

  output$returnsplot<- renderPlot({
    seriesPlot( returns()[, input$symbol2],
          ylim=c(min(min(returns()[, input$symbol2]),
                     min(returns()[, input$symbol3])),
                 max(max(returns()[, input$symbol2]),
                     max(returns()[, input$symbol3]))))
    lines(returns()[, input$symbol3],
          col="blue")
  })

  output$rsplot<- renderPlot({
    plot(prices()[, input$symbol2]/prices()[, input$symbol3])
  })

  ## pairs tab outputs - End

})
