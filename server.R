#
# server.R for Portfolio Optimization
#
# Make sure a package is at least some version (only installs from CRAN)
ensure_version <- function(pkg, ver = "0.0") {
  if (system.file(package = pkg)  == "" || packageVersion(pkg) < ver)
    install.packages(pkg)
}

ensure_version("shiny", "1.0.3")
ensure_version("shinydashboard", "0.5.3")
ensure_version("fPortfolio", "3011.81")
ensure_version("knitr", "1.15.1")
ensure_version("timeSeries", "3022.101.2")
ensure_version("pastecs", "1.3-18")
ensure_version("fMultivar", "3011.78")

library(shiny)
library(shinydashboard)
library(timeSeries)
library(knitr)
library(fPortfolio)
library(pastecs)
library(fMultivar)

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll lower limit to 1MB.
options(shiny.maxRequestSize = 1*1024^2)

# Texto que finaliza en _TEXT se encuentra
# definido en text.R
source("text.R")
# Optiones o configuración que finalizan
# en _CONF se encuentran en config.R
source("config.R")

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

  returns<- reactive({
    #returnsresult<-exp(diff(log(prices()))) - 1
    ##returnsresult<-diff(log(prices()))
    timeSeries::returns(prices(), method=input$returnsType)
  })

  symbols<- function() {
    names(prices())
  }

  cumulated<- reactive({
    # Note, the function cumulated assumes as
    # input discrete returns from a price or index series.
    # see ?cumulated
    timeSeries::cumulated(
      timeSeries::returns(prices(), method="discrete"),
      method=input$cumreturnsType)
  })

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

  output$summarypricestext<- renderPrint({
    summary(prices())
  })

  output$summaryreturnstext<- renderPrint({
    summary(returns())
  })

  output$plotprices<- renderPlot({
    plot(prices(), main=PRICELABEL_TEXT)
  })

  output$plotreturns<- renderPlot({
    plot(returns(), main=PRICELABEL_TEXT)
  })

  output$plotreturnsvisual<- renderPlot({
    fAssets::assetsBasicStatsPlot(returns(), title="", description = "")
  })

  output$plotreturnsmomentsvisual<- renderPlot({
    fAssets::assetsMomentsPlot(returns(), title="", description = "")
  })

  output$plotreturnboxstatsvisual<- renderPlot({
    fAssets::assetsBoxStatsPlot(returns(), title="", description = "")
  })

  histPanel<-function(x, ...) {
    usr<- par("usr")
    on.exit(par(usr))
    par(usr=c(usr[1:2], 0, 1.5))
    h<-hist(x, plot=FALSE)
    breaks<-h$breaks
    nB<-length(breaks)
    y<-h$counts
    y<- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, ...)
  }

  output$pairsplot<- renderPlot({
    fAssets::assetsPairsPlot(returns(), diag.panel=histPanel,
                             pch=19, cex=0.5, col="royalblue4",
                             tick=0, col.axis="white")
  })

  output$corgramplot<- renderPlot({
    fAssets::assetsCorgramPlot(returns(), method="shade", pch=19, cex=0.5)
  })

  output$cortestplot<- renderPlot({
    fAssets::assetsCorTestPlot(returns(), cex=1)
  })

  output$corimageplot<- renderPlot({
    fAssets::assetsCorImagePlot(returns(), cex=1)
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

  # You can use many estimators for Cov
  # covEstimator uses standard covariance estimation,
  # mveEstimator uses the function "cov.mve" from the MASS package,
  # mcdEstimator uses the function "cov.mcd" from the MASS package,
  # lpmEstimator returns lower partial moment estimator,
  # kendallEstimator returns Kendall's rank estimator,
  # spearmanEstimator returns Spearman's rankestimator,
  # covMcdEstimator requires "covMcd" from package robustbase,
  # covOGKEstimator requires "covOGK" from package robustbase,
  # nnveEstimator uses builtin from package covRobust,
  # shrinkEstimator uses builtin from package corpcor.
  # TODO: use differente estimators
  covData<- reactive ({
    fun<-switch(input$covtype,
                "covEstimator"=covEstimator,
                "covEstimator"=covEstimator,
                "mveEstimator"=mveEstimator,
                "mcdEstimator"=mcdEstimator,
                "lpmEstimator"=lpmEstimator,
                "kendallEstimator"=kendallEstimator,
                "spearmanEstimator"=spearmanEstimator,
                "covMcdEstimator"=covMcdEstimator,
                "covOGKEstimator"=covOGKEstimator,
                "nnveEstimator"=nnveEstimator,
                "shrinkEstimator"=shrinkEstimator)
    fun(returns())
  })

  output$covestimator <- renderPrint({
    covData()
  })

  output$symbollist4<- renderUI({
    selectInput("symbol4", "", choices = symbols())
  })

  output$drawdownstable<- renderTable({
    drawdownsStats(returns()[,input$symbol4])
  })

  covDataforOutliers<- reactive ({
    fun<-switch(input$covtypeforoutliers,
                "covEstimator"=covEstimator,
                "covEstimator"=covEstimator,
                "mveEstimator"=mveEstimator,
                "mcdEstimator"=mcdEstimator,
                "lpmEstimator"=lpmEstimator,
                "kendallEstimator"=kendallEstimator,
                "spearmanEstimator"=spearmanEstimator,
                "covMcdEstimator"=covMcdEstimator,
                "covOGKEstimator"=covOGKEstimator,
                "nnveEstimator"=nnveEstimator,
                "shrinkEstimator"=shrinkEstimator)
    fun(returns())
  })

  output$outlierstext<- renderPrint({
    fAssets::assetsOutliers(returns(),
                            timeSeries::colMeans(returns()),
                            covDataforOutliers()$Sigma)
  })

  amcov1<- reactive ({
    # TODO: por qué por 100??
    fAssets::assetsMeanCov(returns(), method = input$meancovmethod1)
  })

  amcov2<- reactive ({
    # TODO: por qué por 100??
    fAssets::assetsMeanCov(returns(), method = input$meancovmethod2)
  })

  output$meancovtext1<- renderPrint({
    amcov1()
  })

  output$meancovtext2<- renderPrint({
    amcov2()
  })

  output$covellipsesplot<- renderPlot({
    fAssets::covEllipsesPlot((list(amcov1()$cov, amcov2()$cov)))
    title(main = paste0(input$meancovmethod1, " vs. ", input$meancovmethod2," Covariances"))
  })

  ## stats tab outputs - End

  ## group tab outputs - Begin

  ahclust<- function() {
    fAssets::assetsSelect(returns(), method="hclust")
  }

  output$hclustplot<- renderPlot({
    plot(ahclust())
  })

  output$hclusttext<- renderPrint({
    ahclust()
  })

  output$clustersui<- renderUI({
    sliderInput("clusters", label="", min=1, max =length(symbols())-1, value = 2, step = 1)
  })

  akmeans<- function() {
    fAssets::assetsSelect(returns(), method="kmeans",
                          control=c(centers=input$clusters, algorithm="Hartigan-Wong"))
  }

  output$kmeanstext<- renderPrint({
    sort(akmeans()$cluster)
  })

  output$coreigenplot<- renderPlot({
    fAssets::assetsCorEigenPlot(returns(), method="kendall")
  })


  ## group tab outputs - End

  ## optimize tab outputs - Begin

  createPortSpec<- function(nullmu, mu, nullrisk, risk, riskfreerate,
                            porttype, portoptimize, portcovest,
                            varalpha, lpmriskmeasureexponent, solver) {
    if (nullmu) {
      mui<-NULL
    } else {
      mui<-mu
    }
    if (nullrisk) {
      riski<-NULL
    } else {
      riski<-risk
    }
    portfolioSpec(
      model = list(
        type=porttype,
        optimize=portoptimize,
        estimator=portcovest,
        tailRisk=list(),
        params=list(
          alpha=as.double(varalpha),
          a=as.integer(lpmriskmeasureexponent)
        )
      ),
      portfolio = list(
        weights=NULL,
        targetReturn=mui,
        targetRisk=riski,
        riskFreeRate=riskfreerate,
        nFrontierPoints=50,
        status=NA
      ),
      optim = list(
        solver=solver,
        objective=NULL,
        options=list(),
        control=list(),
        trace=FALSE
      )
    )
  }

  psep<-reactive({
    createPortSpec(input$nullmuep,
                   input$targetmuep,
                   input$nullriskep,
                   input$targetriskep,
                   input$riskfreerateep,
                   input$porttypeep,
                   input$portoptimizeep,
                   input$portcovestep,
                   input$varalphaep,
                   input$lpmriskmeasureexponentep,
                   input$solverep)
  })

  # TODO: usar spec diferentes y constrains diferentes
  frontierCalcep <- reactive({
    portfolioFrontier(returns(), spec = psep(), constraints = input$constrainsep)
  })

  output$plotep <- renderPlot({
    frontierPlot(frontierCalcep(), frontier = "both", risk="Sigma", type="l")
    minvariancePoints(frontierCalcep(), pch=19, col="red")
    singleAssetPoints(frontierCalcep(), risk = "Sigma", pch=19, cex=1.5, col=topo.colors(6))
  })

  output$wplotep <- renderPlot({
    weightsPlot(frontierCalcep())
  })

  output$optimizeparamsep<- renderPrint({
    print(psep())
  })

  col<- function(p) {
    qualiPalette(ncol(returns()), p)
  }

  mtextop<- function(ps) {
    mtext(text = paste0(ps@model$optimize, " ",
                        ps@model$type, " Portfolio"),
          side = 3,
          line = 1.5,
          font = 2,
          cex = 0.7,
          adj = 0)
  }

  ep<- reactive({
    efficientPortfolio(returns(), spec = psep(), constraints = input$constrainsep)
  })

  output$textep<- renderPrint({
    # efficientPortfolio returns the portfolio with the lowest
    # risk for a given target return
    ep()
  })

  output$wPieep<- renderPlot({
    weightsPie(ep(), radius = 0.7, col = col("Dark2"))
    mtextop(psep())
  })

  output$wRetPieep<- renderPlot({
    weightedReturnsPie(ep(), radius = 0.7, col = col("Dark2"))
    mtextop(psep())
  })

  output$covRiskBPieep<- renderPlot({
    covRiskBudgetsPie(ep(), radius = 0.7, col = col("Dark2"))
    mtextop(psep())
  })

  pstp<-reactive({
    createPortSpec(input$nullmutp,
                   input$targetmutp,
                   input$nullrisktp,
                   input$targetrisktp,
                   input$riskfreeratetp,
                   input$porttypetp,
                   input$portoptimizetp,
                   input$portcovesttp,
                   input$varalphatp,
                   input$lpmriskmeasureexponenttp,
                   input$solvertp)
  })

  frontierCalctp <- reactive({
    portfolioFrontier(returns(), spec = pstp(), constraints = input$constrainstp)
  })

  output$plottp <- renderPlot({
    frontierPlot(frontierCalctp(), frontier = "both", risk="Sigma", type="l")
    minvariancePoints(frontierCalctp(), pch=19, col="red")
    singleAssetPoints(frontierCalctp(), risk = "Sigma", pch=19, cex=1.5, col=topo.colors(6))
  })

  output$wplottp <- renderPlot({
    weightsPlot(frontierCalctp())
  })

  output$optimizeparamstp<- renderPrint({
    print(pstp())
  })

  tp<- reactive({
    # maxratioPortfolio returns the portfolio with the highest
    # return/risk ratio
    # tangencyPortfolio synonym for maxratioPortfolio
    tangencyPortfolio(returns(), spec = pstp(), constraints = input$constrainstp)
  })

  output$texttp<- renderPrint({
    tp()
  })

  output$wPietp<- renderPlot({
    weightsPie(tp(), box = FALSE, col = col("Paired"))
    mtextop(pstp())
  })

  output$wRetPietp<- renderPlot({
    weightedReturnsPie(tp(), box = FALSE, col = col("Paired"))
    mtextop(pstp())
  })

  output$covRiskBPietp<- renderPlot({
    covRiskBudgetsPie(tp(), box = FALSE, col = col("Paired"))
    mtextop(pstp())
  })

  psmv<-reactive({
    createPortSpec(input$nullmumv,
                   input$targetmumv,
                   input$nullriskmv,
                   input$targetriskmv,
                   input$riskfreeratemv,
                   input$porttypemv,
                   input$portoptimizemv,
                   input$portcovestmv,
                   input$varalphamv,
                   input$lpmriskmeasureexponentmv,
                   input$solvermv)
  })

  frontierCalcmv <- reactive({
    portfolioFrontier(returns(), spec = psmv(), constraints = input$constrainsmv)
  })

  output$plotmv <- renderPlot({
    frontierPlot(frontierCalcmv(), frontier = "both", risk="Sigma", type="l")
    minvariancePoints(frontierCalcmv(), pch=19, col="red")
    singleAssetPoints(frontierCalcmv(), risk = "Sigma", pch=19, cex=1.5, col=topo.colors(6))
  })

  output$wplotmv <- renderPlot({
    weightsPlot(frontierCalcmv())
  })

  output$optimizeparamsmv<- renderPrint({
    print(psmv())
  })

  mv<- reactive({
    # minriskPortfolio returns a portfolio with the lowest
    # risk at all
    # minvariancePortfolio synonym for minriskPortfolio
    minvariancePortfolio(returns(), spec = psmv(), constraints = input$constrainsmv)
  })

  output$textmv <- renderPrint({
    mv()
  })

  output$wPiemv<- renderPlot({
    weightsPie(mv(), box = FALSE, col = col("Accent"))
    mtextop(psmv())
  })

  output$wRetPiemv<- renderPlot({
    weightedReturnsPie(mv(), box = FALSE, col = col("Accent"))
    mtextop(psmv())
  })

  output$covRiskBPiemv<- renderPlot({
    covRiskBudgetsPie(mv(), box = FALSE, col = col("Accent"))
    mtextop(psmv())
  })

  psmr<-reactive({
    createPortSpec(input$nullmumr,
                   input$targetmumr,
                   input$nullriskmr,
                   input$targetriskmr,
                   input$riskfreeratemr,
                   input$porttypemr,
                   input$portoptimizemr,
                   input$portcovestmr,
                   input$varalphamr,
                   input$lpmriskmeasureexponentmr,
                   input$solvermr)
  })

  frontierCalcmr <- reactive({
    portfolioFrontier(returns(), spec = psmr(), constraints = input$constrainsmr)
  })

  output$plotmr <- renderPlot({
    frontierPlot(frontierCalcmr(), frontier = "both", risk="Sigma", type="l")
    minvariancePoints(frontierCalcmr(), pch=19, col="red")
    singleAssetPoints(frontierCalcmr(), risk = "Sigma", pch=19, cex=1.5, col=topo.colors(6))
  })

  output$wplotmr <- renderPlot({
    weightsPlot(frontierCalcmr())
  })

  output$optimizeparamsmr<- renderPrint({
    print(psmr())
  })

  mr<- reactive({
    # maxreturnPortfolio returns the portfolio with the highest
    # return for a given target risk
    maxreturnPortfolio(returns(), spec = psmr(), constraints = input$constrainsmr)
  })

  output$textmr <- renderPrint({
    mr()
  })

  output$wPiemr<- renderPlot({
    weightsPie(mr(), box = FALSE, col = col("Set1"))
    mtextop(psmr())
  })

  output$wRetPiemr<- renderPlot({
    weightedReturnsPie(mr(), box = FALSE, col = col("Set1"))
    mtextop(psmr())
  })

  output$covRiskBPiemr<- renderPlot({
    covRiskBudgetsPie(mr(), box = FALSE, col = col("Set1"))
    mtextop(psmr())
  })

  ## optimize tab outputs - End


  ## individual tab outputs - Begin

  output$symbollist<- renderUI({
    selectInput("symbol", INSTRSEL_TEXT,
                choices = symbols()
    )
  })

  output$meanvalue<- renderValueBox({
    valueBox(
      round(mean(returns()[, input$symbol]), digits = 6),
      MEAN_TEXT, icon = icon("balance-scale")
    )
  })

  output$varvalue<- renderValueBox({
    valueBox(
      round(stdev(returns()[, input$symbol])^2, digits = 6),
      VAR_TEXT, icon = icon("line-chart"),
      color = "purple"
    )
  })

  output$stddevvalue<- renderValueBox({
    valueBox(
      round(stdev(returns()[, input$symbol]), digits = 6),
      STDDEV_TEXT, icon = icon("arrows-h"),
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
    h<-histPlot(rdata, title = FALSE)
    #xfit<-seq(min(rdata),max(rdata),length=100)
    #yfit<-dnorm(xfit,mean=mean(rdata),sd=sd(rdata))
    #yfit <- yfit*diff(h$mids[1:2])*length(rdata)
    #lines(xfit, yfit, col="blue", lwd=2)
  })

  output$returnsdensityplot<- renderPlot({
    densityPlot(returns()[, input$symbol], title=FALSE)
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

  output$cumulatedplot<- renderPlot({
    fBasics::cumulatedPlot(returns()[, input$symbol], main=CUMULATED_TEXT)
  })

  output$drawdownsdplot<- renderPlot({
    fBasics::drawdownPlot(returns()[, input$symbol])
  })

  output$pricesboxplot<- renderPlot({
    fBasics::boxPlot(prices()[, input$symbol], title = FALSE)
  })

  output$returnsboxplot<- renderPlot({
    fBasics::boxPlot(returns()[, input$symbol], title = FALSE)
  })

  ## individual tab outputs - End

  ## pairs tab outputs - Begin

  output$symbollist2<- renderUI({
    selectInput("symbol2", "", choices = symbols())
  })

  output$symbollist3<- renderUI({
    selectInput("symbol3", "", choices = symbols())
  })

  output$covarvalue<- renderValueBox({
    valueBox(
      round(cov(returns()[, input$symbol2], returns()[, input$symbol3]), digits = 6),
      COVAR_TEXT, icon = icon("line-chart"),
      color = "purple"
    )
  })

  output$correlvalue<- renderValueBox({
    valueBox(
      round(cor(returns()[, input$symbol2], returns()[, input$symbol3]), digits = 6),
      COR_TEXT, icon = icon("arrows-h"),
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

  output$scatterreturnsplot<- renderPlot({
    plot(as.vector(returns()[, input$symbol2]), as.vector(returns()[, input$symbol3]),
         xlab=input$symbol2, ylab=input$symbol3, type="p", pch=16)
  })

  output$hexbinplot<- renderPlot({
    hexHist<-hexBinning(returns()[,c(input$symbol2,input$symbol3)], bin=20)
    plot(hexHist, xlab=input$symbol2, ylab=input$symbol3, col=rev(greyPalette(20)))
  })

  ## pairs tab outputs - End

})
