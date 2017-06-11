#
# ui.R for Portfolio Optimization
#
# Make sure a package is at least some version (only installs from CRAN)
ensure_version <- function(pkg, ver = "0.0") {
  if (system.file(package = pkg)  == "" || packageVersion(pkg) < ver)
    install.packages(pkg)
}

ensure_version("shiny", "1.0.3")
ensure_version("shinydashboard", "0.5.3")

library(shiny)
library(shinydashboard)

# Texto que finaliza en _TEXT se encuentra
# definido en text.R
source("text.R")
# Optiones o configuración que finalizan
# en _CONF se encuentran en config.R
source("config.R")

header<-dashboardHeader(
  title = tags$img(src="img/vision.png", width=100))

menu<-dashboardSidebar(
  sidebarMenu(
    id = "menuitems",
    h4(APPTITLE_TEXT, style="text-align:center;"),
    menuItem(DATAMENUTITLE_TEXT, tabName = "data", icon=icon("table"), selected = TRUE),
    menuItem(STATSMENUTITLE_TEXT, tabName = "stats", icon=icon("pie-chart")),
    menuItem(CLUSTERMENUTITLE_TEXT, tabName="group", icon=icon("object-group")),
    menuItem(OPTIMIZEMENUTITLE_TEXT, tabName = "optimize", icon=icon("area-chart")),
    menuItem(INDIVMENUTITLE_TEXT, tabName="individual", icon=icon("file-o")),
    menuItem(PAIRSMENUTITLE_TEXT, tabName="pairs", icon=icon("files-o"))
  )
)

datatab<-tabItem(
  tabName = "data",
  fluidRow(
    box(width = 3, title = UPLOADDATA_TEXT,
        fileInput('file_w_data', SELECTFILE_TEXT,
                  accept = UPLOADFILETYPE_CONF
        ),
        tags$hr(),
        checkboxInput('header', WITHHEADER_TEXT, TRUE),
        radioButtons('sep', SEPARATOR_TEXT,
                     UPLOADFILESEP_CONF, ','),
        radioButtons('quote', COMILLAS_TEXT,
                     UPLOADCOMILLAS_CONF, ''),
        tags$hr(),
        p(UPLOADHELP_TEXT1,
          a(href = DEMOFILEURL_CONF, DEMOFILETEXT_CONF),
          UPLOADHELP_TEXT2)
    ),
    tabBox(
      width = 9, title = SYMBOLSTABTITLE_TEXT,
      tabPanel(
        icon = icon("check-circle"), title = SYMBOLSLIST_TEXT,
        tableOutput('datatable')
      ),
      tabPanel(
        icon = icon("check-circle"),title = RETURNSTAB_TEXT,
        fluidRow(
          box(radioButtons(inputId = "returnsType",
                           label = RETURNTYPELABEL_TEXT,
                           choices = RETURNTYPE_CONF), width = 12)
        ),
        tableOutput('returntable')
      ),
      tabPanel(
        icon = icon("check-circle"), title = CUMRETURNTITLE_TEXT,
        fluidRow(
          box(radioButtons(inputId = "cumreturnsType",
                           label = RETURNTYPELABEL_TEXT,
                           choices = RETURNTYPE_CONF), width = 12)
        ),
        tableOutput('cumulatedtable')
      ),
      tabPanel(
        icon = icon("check-circle"), title = DRAWDOWNTITLE_TEXT,
        tableOutput('drawdowstable')
      )
    )
  )
)

statstab<-tabItem(
  tabName = "stats",
  fluidRow(
    tabBox(
      width = 12, title = GENERALSTATSTITLE_TEXT,
      tabPanel(
        icon = icon("check-circle"), title = PRICERETSUMMARYTABTITLE_TEXT,
        h2(SUMMARYPRICEHEADER_TEXT),
        verbatimTextOutput("summarypricestext"),
        h2(SUMMARYRETHEADER_TEXT),
        verbatimTextOutput("summaryreturnstext"),
        h2(SUMMARYPRICECHARTHEAD_TEXT),
        plotOutput("plotprices"),
        h2(SUMMARYRETCHARTHEAD_TEXT),
        plotOutput("plotreturns")
      ),
      tabPanel(
        icon = icon("check-circle"), title = BASICSTATSTABTITLE_TEXT,
        fluidRow(
          tabsetPanel(
            tabPanel(SUMMARYRETCHARTVIS_TEXT,
              plotOutput("plotreturnsvisual")
            ),
            tabPanel(RETMOMENTSCHARTVIS_TEXT,
              plotOutput("plotreturnsmomentsvisual")
            ),
            tabPanel(RETBOXSTATSCHARTVIS_TEXT,
              plotOutput("plotreturnboxstatsvisual")
            ),
            tabPanel(PAIRSPLOT_TEXT,
              plotOutput("pairsplot")
            ),
            tabPanel(CORGRAMPLOT_TEXT,
              plotOutput("corgramplot")
            ),
            tabPanel(CORTESTPLOT_TEXT,
              plotOutput("cortestplot")
            ),
            tabPanel(CORIMAGEPLOT_TEXT,
              plotOutput("corimageplot")
            )
          ),
          box(width = 12, title = BASICPRICESTATS_TEXT,
            verbatimTextOutput("basicstatspricestext")
          ),
          box(width = 12, title = BASICRETSTATS_TEXT,
            verbatimTextOutput("basicstatsreturnstext")
          ),
          box(width = 12, title = RETQUANTILETITLE_TEXT,
              selectInput(inputId = "quantiletype",
                          label = CALCTYPE_TEXT,
                          choices = QUANTILECALCTYPE_CONF
                          ),
              verbatimTextOutput("returnsquantilestext")
          ),
          box(width = 12, title = INSTRRETQUANTILE_TEXT,
              fluidRow(
                box(width = 6, title = "",
                sliderInput(inputId = "quantileprob",
                            label = "Cuantil",
                            min = 0, max = 1, value = 0.05, step = 0.01
                            )
                ),
                box(width = 6, title = "",
                selectInput(inputId = "quantiletype2",
                          label = "Tipo de cálculo",
                          choices = QUANTILECALCTYPE_CONF
                          )
                )
              ),
              verbatimTextOutput("returnscolquantilestext")
          )
        )
      ),
      tabPanel(
        icon = icon("check-circle"), title = VARCOVARTABTITLE_TEXT,
        fluidRow(
          box(width = 12, title = COVARESTTABTITLE_TEXT,
              selectInput(inputId = "covtype",
                          label = ESTLABEL_TEXT,
                          choices = COVESTIMATOR_CONF
              )
          ),
          box(width = 12, title = ESTRESULT_TEXT,
              verbatimTextOutput("covestimator")
          )
        )
      ),
      tabPanel(
        icon = icon("check-circle"), title = DRAWDTABTITLE_TEXT,
        fluidRow(
          box(width = 12, title = INSTRSEL_TEXT,
              uiOutput("symbollist4")
          )
        ),
        fluidRow(
          box(width = 12, title = DRAWDPERIODS_TEXT,
              tableOutput("drawdownstable")
          )
        )
      ),
      tabPanel(
        icon = icon("check-circle"), title = OUTLIERTABTITLE_TEXT,
        fluidRow(
          box(width = 12, title = COVARESTTABTITLE_TEXT,
              selectInput(inputId = "covtypeforoutliers",
                          label = ESTLABEL_TEXT,
                          choices = COVESTIMATOR_CONF
              )
          ),
          box(width = 12, title = OUTLIERVALUES_TEXT,
              verbatimTextOutput("outlierstext")
          )
        )
      ),
      tabPanel(
        icon = icon("check-circle"), title = ROBUSTTABTITLE_TEXT,
        fluidRow(
          box(width = 6, title = "",
              selectInput(inputId = "meancovmethod1",
                          label = CALCMETHODLABEL_TEXT,
                          choices = COVCALCTYPE_CONF
              )
          ),
          box(width = 6, title = "",
              selectInput(inputId = "meancovmethod2",
                          label = CALCMETHODLABEL_TEXT,
                          choices = COVCALCTYPE_CONF
              )
          ),
          box(width = 12, title = COMPROBUSTCOVAR_TEXT,
              plotOutput("covellipsesplot")
          ),
          box(width = 6, title = ROBUSTCOVAR1_TEXT,
              verbatimTextOutput("meancovtext1")
          ),
          box(width = 6, title = ROBUSTCOVAR2_TEXT,
              verbatimTextOutput("meancovtext2")
          )
        )
      )
    )
  )
)

grouptab<-tabItem(
  tabName = "group",
  fluidRow(
    tabBox(
      width = 12, title = GROUPSTABTITLE_TEXT,
      tabPanel(
        icon = icon("check-circle"), title = HCLUSTTABTITLE_TEXT,
        fluidRow(
          box(width = 12, title = HCLUSTPLOTTITLE_TEXT,
              plotOutput("hclustplot")
          ),
          box(width = 12, title = HCLUSTTEXTTITLE_TEXT,
              verbatimTextOutput("hclusttext")
          )
        )
      ),
      tabPanel(
        icon = icon("check-circle"), title = KMEANSTABTITLE_TEXT,
        fluidRow(
          box(width = 12, title = KMEANSPLOTTITLE_TEXT,
              verbatimTextOutput("kmeanstext")
          )
        )
      ),
      tabPanel(
        icon = icon("check-circle"), title = EIGENTABTITLE_TEXT,
        fluidRow(
          box(width = 12, title = EIGENPLOTTITLE_TEXT,
              plotOutput("coreigenplot")
          )
        )
      )
    )
  )
)

optimizetab<-tabItem(
  tabName = "optimize",
  fluidRow(
    tabBox(
      width = 12, title = OPTIMIZETABTITLE_TEXT,
      tabPanel(
        icon = icon("check-circle"), title = EFICIENTPORTFOLIO_TEXT,
        tabsetPanel(
          tabPanel(
            title = CHARTS_TEXT,
            fluidRow(
              box(width = 12, title = EFFICIENTFRONTIER_TEXT,
                  plotOutput("plotep")
              ),
              box(width = 12, title = WEIGHTSCHART_TEXT,
                  plotOutput("wplotep")
              )
            )
          ),
          tabPanel(
            title = PORTFOLIO_TEXT,
            fluidRow(
              box(width=12, title="",
                  verbatimTextOutput("textep")
              ),
              box(width = 12, title = "",
                  verbatimTextOutput("optimizeparamsep")
              )
            )
          ),
          tabPanel(
            title = PARAMS_TEXT,
            fluidRow(
              box(width = 3, title = "",
                  selectInput(inputId = "porttypeep",
                              label = PORTTYPE_TEXT,
                              choices = PORTTYPE_CONF
                  )
              ),
              box(width = 3, title = "",
                  selectInput(inputId = "portoptimizeep",
                              label = PORTOPTIMIZE_TEXT,
                              choices = PORTOPTIMIZE_CONF
                  )
              ),
              box(width = 3, title = "",
                  selectInput(inputId = "portcovestep",
                              label = PORTCOVESTIMATOR_TEXT,
                              choices = PORTCOVESTIMATOR_CONF
                  )
              ),
              box(width = 3, title = "",
                  sliderInput("varalphaep", label=VARALPHA_TEXT, min=0.8, max = 1, value = 0.95, step = 0.01)
              )
            ),
            fluidRow(
              box(width = 3, title = "",
                  sliderInput("lpmriskmeasureexponentep", label=LPMRISKEXP_TEXT, min=1, max = 10, value = 1, step = 1)
              ),
              box(width = 3, title = "",
                  selectInput(inputId = "solverep",
                              label = SOLVER_TEXT,
                              choices = SOLVER_CONF
                  )
              ),
              box(width = 3, title = "",
                  checkboxInput("nullmuep",label = NULLRETURN_TEXT, value = TRUE)
              ),
              box(width = 3, title = "",
                  sliderInput("targetmuep", label = RETURN_TEXT, min=0, max = 3, value = 0.05, step = 0.01)
              )
            ),
            fluidRow(
              box(width = 3, title = "",
                  checkboxInput("nullriskep",label = NULLRISK_TEXT, value = TRUE)
              ),
              box(width = 3, title = "",
                  sliderInput("targetriskep", label = RISK_TEXT, min=0, max = 3, value = 0.05, step = 0.01)
              ),
              box(width = 3, title = "",
                  sliderInput("riskfreerateep", label = RISKFREE_TEXT, min=0, max = 0.1, value = 0, step = 0.005)
              ),
              box(width = 3, title = "",
                  selectInput(inputId = "constrainsep",
                              label = CONSTRAINS_TEXT,
                              choices = CONSTRAINS_CONF
                  )
              )
            )
          )
        )
      ),
      tabPanel(
        icon = icon("check-circle"), title = TANGENCYPORTFOLIO_TEXT,
        tabsetPanel(
          tabPanel(
            title = CHARTS_TEXT,
            fluidRow(
              box(width = 12, title = EFFICIENTFRONTIER_TEXT,
                  plotOutput("plottp")
              ),
              box(width = 12, title = WEIGHTSCHART_TEXT,
                  plotOutput("wplottp")
              )
            )
          ),
          tabPanel(
            title = PORTFOLIO_TEXT,
            fluidRow(
              box(width=12, title="",
                  verbatimTextOutput("texttp")
              ),
              box(width = 12, title = "",
                  verbatimTextOutput("optimizeparamstp")
              )
            )
          ),
          tabPanel(
            title = PARAMS_TEXT,
            fluidRow(
              box(width = 3, title = "",
                  selectInput(inputId = "porttypetp",
                              label = PORTTYPE_TEXT,
                              choices = PORTTYPE_CONF
                  )
              ),
              box(width = 3, title = "",
                  selectInput(inputId = "portoptimizetp",
                              label = PORTOPTIMIZE_TEXT,
                              choices = PORTOPTIMIZE_CONF
                  )
              ),
              box(width = 3, title = "",
                  selectInput(inputId = "portcovesttp",
                              label = PORTCOVESTIMATOR_TEXT,
                              choices = PORTCOVESTIMATOR_CONF
                  )
              ),
              box(width = 3, title = "",
                  sliderInput("varalphatp", label=VARALPHA_TEXT, min=0.8, max = 1, value = 0.95, step = 0.01)
              )
            ),
            fluidRow(
              box(width = 3, title = "",
                  sliderInput("lpmriskmeasureexponenttp", label=LPMRISKEXP_TEXT, min=1, max = 10, value = 1, step = 1)
              ),
              box(width = 3, title = "",
                  selectInput(inputId = "solvertp",
                              label = SOLVER_TEXT,
                              choices = SOLVER_CONF
                  )
              ),
              box(width = 3, title = "",
                  checkboxInput("nullmutp",label = NULLRETURN_TEXT, value = TRUE)
              ),
              box(width = 3, title = "",
                  sliderInput("targetmutp", label = RETURN_TEXT, min=0, max = 3, value = 0.05, step = 0.01)
              )
            ),
            fluidRow(
              box(width = 3, title = "",
                  checkboxInput("nullrisktp",label = NULLRISK_TEXT, value = TRUE)
              ),
              box(width = 3, title = "",
                  sliderInput("targetrisktp", label = RISK_TEXT, min=0, max = 3, value = 0.05, step = 0.01)
              ),
              box(width = 3, title = "",
                  sliderInput("riskfreeratetp", label = RISKFREE_TEXT, min=0, max = 0.1, value = 0, step = 0.005)
              ),
              box(width = 3, title = "",
                  selectInput(inputId = "constrainstp",
                              label = CONSTRAINS_TEXT,
                              choices = CONSTRAINS_CONF
                  )
              )
            )
          )
        )
      ),
      tabPanel(
        icon = icon("check-circle"), title = MINVARPORTFOLIO_TEXT,
        tabsetPanel(
          tabPanel(
            title = CHARTS_TEXT,
            fluidRow(
              box(width = 12, title = EFFICIENTFRONTIER_TEXT,
                  plotOutput("plotmv")
              ),
              box(width = 12, title = WEIGHTSCHART_TEXT,
                  plotOutput("wplotmv")
              )
            )
          ),
          tabPanel(
            title = PORTFOLIO_TEXT,
            fluidRow(
              box(width=12, title="",
                  verbatimTextOutput("textmv")
              ),
              box(width = 12, title = "",
                  verbatimTextOutput("optimizeparamsmv")
              )
            )
          ),
          tabPanel(
            title = PARAMS_TEXT,
            fluidRow(
              box(width = 3, title = "",
                  selectInput(inputId = "porttypemv",
                              label = PORTTYPE_TEXT,
                              choices = PORTTYPE_CONF
                  )
              ),
              box(width = 3, title = "",
                  selectInput(inputId = "portoptimizemv",
                              label = PORTOPTIMIZE_TEXT,
                              choices = PORTOPTIMIZE_CONF
                  )
              ),
              box(width = 3, title = "",
                  selectInput(inputId = "portcovestmv",
                              label = PORTCOVESTIMATOR_TEXT,
                              choices = PORTCOVESTIMATOR_CONF
                  )
              ),
              box(width = 3, title = "",
                  sliderInput("varalphamv", label=VARALPHA_TEXT, min=0.8, max = 1, value = 0.95, step = 0.01)
              )
            ),
            fluidRow(
              box(width = 3, title = "",
                  sliderInput("lpmriskmeasureexponentmv", label=LPMRISKEXP_TEXT, min=1, max = 10, value = 1, step = 1)
              ),
              box(width = 3, title = "",
                  selectInput(inputId = "solvermv",
                              label = SOLVER_TEXT,
                              choices = SOLVER_CONF
                  )
              ),
              box(width = 3, title = "",
                  checkboxInput("nullmumv",label = NULLRETURN_TEXT, value = FALSE)
              ),
              box(width = 3, title = "",
                  sliderInput("targetmumv", label = RETURN_TEXT, min=0, max = 3, value = 0.05, step = 0.01)
              )
            ),
            fluidRow(
              box(width = 3, title = "",
                  checkboxInput("nullriskmv",label = NULLRISK_TEXT, value = TRUE)
              ),
              box(width = 3, title = "",
                  sliderInput("targetriskmv", label = RISK_TEXT, min=0, max = 3, value = 0.05, step = 0.01)
              ),
              box(width = 3, title = "",
                  sliderInput("riskfreeratemv", label = RISKFREE_TEXT, min=0, max = 0.1, value = 0, step = 0.005)
              ),
              box(width = 3, title = "",
                  selectInput(inputId = "constrainsmv",
                              label = CONSTRAINS_TEXT,
                              choices = CONSTRAINS_CONF
                  )
              )
            )
          )
        )
      ),
      tabPanel(
        icon = icon("check-circle"), title = MAXRETURNPORTFOLIO_TEXT,
        tabsetPanel(
          tabPanel(
            title = CHARTS_TEXT,
            fluidRow(
              box(width = 12, title = EFFICIENTFRONTIER_TEXT,
                  plotOutput("plotmr")
              ),
              box(width = 12, title = WEIGHTSCHART_TEXT,
                  plotOutput("wplotmr")
              )
            )
          ),
          tabPanel(
            title = PORTFOLIO_TEXT,
            fluidRow(
              box(width=12, title="",
                  verbatimTextOutput("textmr")
              ),
              box(width = 12, title = "",
                  verbatimTextOutput("optimizeparamsmr")
              )
            )
          ),
          tabPanel(
            title = PARAMS_TEXT,
            fluidRow(
              box(width = 3, title = "",
                  selectInput(inputId = "porttypemr",
                              label = PORTTYPE_TEXT,
                              choices = PORTTYPE_CONF
                  )
              ),
              box(width = 3, title = "",
                  selectInput(inputId = "portoptimizemr",
                              label = PORTOPTIMIZE_TEXT,
                              choices = PORTOPTIMIZE_CONF
                  )
              ),
              box(width = 3, title = "",
                  selectInput(inputId = "portcovestmr",
                              label = PORTCOVESTIMATOR_TEXT,
                              choices = PORTCOVESTIMATOR_CONF
                  )
              ),
              box(width = 3, title = "",
                  sliderInput("varalphamr", label=VARALPHA_TEXT, min=0.8, max = 1, value = 0.95, step = 0.01)
              )
            ),
            fluidRow(
              box(width = 3, title = "",
                  sliderInput("lpmriskmeasureexponentmr", label=LPMRISKEXP_TEXT, min=1, max = 10, value = 1, step = 1)
              ),
              box(width = 3, title = "",
                  selectInput(inputId = "solvermr",
                              label = SOLVER_TEXT,
                              choices = SOLVER_CONF
                  )
              ),
              box(width = 3, title = "",
                  checkboxInput("nullmumr",label = NULLRETURN_TEXT, value = TRUE)
              ),
              box(width = 3, title = "",
                  sliderInput("targetmumr", label = RETURN_TEXT, min=0, max = 3, value = 0.05, step = 0.01)
              )
            ),
            fluidRow(
              box(width = 3, title = "",
                  checkboxInput("nullriskmr",label = NULLRISK_TEXT, value = FALSE)
              ),
              box(width = 3, title = "",
                  sliderInput("targetriskmr", label = RISK_TEXT, min=0, max = 3, value = 0.05, step = 0.01)
              ),
              box(width = 3, title = "",
                  sliderInput("riskfreeratemr", label = RISKFREE_TEXT, min=0, max = 0.1, value = 0, step = 0.005)
              ),
              box(width = 3, title = "",
                  selectInput(inputId = "constrainsmr",
                              label = CONSTRAINS_TEXT,
                              choices = CONSTRAINS_CONF
                  )
              )
            )
          )
        )
      )
    )
  )
)

individualtab<-tabItem(
  tabName = "individual",
  fluidRow(
    box(width = 12, title = INSTRDATATITLE_TEXT,
        uiOutput("symbollist")
    )
  ),
  fluidRow(
    valueBoxOutput("meanvalue"),
    valueBoxOutput("varvalue"),
    valueBoxOutput("stddevvalue")
  ),
  fluidRow(
    box(width = 6, title = PRICELABEL_TEXT,
        plotOutput("priceplot")
    ),
    box(width = 6, title = RETLABEL_TEXT,
        plotOutput("returnplot")
    )
  ),
  fluidRow(
    box(width = 6, title = RETHIST_TEXT,
        plotOutput("returnshistplot")
    ),
    box(width = 6, title = DENSITYCHART_TEXT,
        plotOutput("returnsdensityplot")
    )
  ),
  fluidRow(
    box(width = 6, title = SMOOTHCHART_TEXT,
        plotOutput('lowessplot')
    ),
    box(width = 6, title = TURNS_TEXT,
      plotOutput('turnsplot')
    )
  ),
  fluidRow(
    box(width = 6, title = CUMULATED_TEXT,
        plotOutput("cumulatedplot")
    ),
    box(width = 6, title = DRAWD_TEXT,
        plotOutput("drawdownsdplot")
    )
  ),
  fluidRow(
    box(width = 6, title = PRICEBOXPLOT_TEXT,
        plotOutput("pricesboxplot")
    ),
    box(width = 6, title = RETBOXPLOT_TEXT,
        plotOutput("returnsboxplot")
    )
  )
)

pairstab<-tabItem(
  tabName = "pairs",
  fluidRow(
    box(width = 6, title = SELECTINSTR1_TEXT,
        uiOutput("symbollist2")
    ),
    box(width = 6, title = SELECTINSTR2_TEXT,
        uiOutput("symbollist3")
    )
  ),
  fluidRow(
      valueBoxOutput("covarvalue", width = 6),
      valueBoxOutput("correlvalue", width = 6)
  ),
  fluidRow(
    box(width = 6, title = PRICELABEL_TEXT,
        plotOutput("pricesplot")
    ),
    box(width = 6, title = RETLABEL_TEXT,
        plotOutput("returnsplot")
    )
  ),
  fluidRow(
    box(width = 6, title = RELATIVESTRENGTH_TEXT,
        plotOutput("rsplot")
    ),
    box(width = 6, title = RETLABEL_TEXT,
        plotOutput("scatterreturnsplot")
    )
  ),
  fluidRow(
    box(width = 6, title = HEXBINPLOT_TEXT,
        plotOutput("hexbinplot")
    )
  )
)

body<-dashboardBody(
  tabItems(datatab, statstab, grouptab,
           optimizetab, individualtab,
           pairstab)
)

# Define UI
shinyUI(
  dashboardPage(
    skin = "green", header, menu, body
  )
)

