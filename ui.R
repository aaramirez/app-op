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

optimizetab<-tabItem(
  tabName = "optimize",
  fluidRow(
    box(width = 12, title = "Frontera eficiente",
        plotOutput("efplot")
    ),
    box(width = 12, title = "Gráfico de pesos",
        plotOutput("wplot")
    ),
    box(width = 12, title = "Portafolio de Varianza Mínima",
        sliderInput("mvmu", label="mu", min=0, max = 3, value = 0.05, step = 0.01),
        verbatimTextOutput("vmtext")
    )
  )
)

individualtab<-tabItem(
  tabName = "individual",
  fluidRow(
    box(width = 12, title = "Datos de los instrumentos",
        uiOutput("symbollist")
    )
  ),
  fluidRow(
    valueBoxOutput("meanvalue"),
    valueBoxOutput("varvalue"),
    valueBoxOutput("stddevvalue")
  ),
  fluidRow(
    box(width = 6, title = "Precio",
        plotOutput("priceplot")
    ),
    box(width = 6, title = "Retornos",
        plotOutput("returnplot")
    )
  ),
  fluidRow(
    box(width = 6, title = "Histograma de retornos",
        plotOutput("returnshistplot")
    ),
    box(width = 6, title = "Densidad",
        plotOutput("returnsdensityplot")
    )
  ),
  fluidRow(
    box(width = 6, title = "Serie suavizada",
        plotOutput('lowessplot')
    ),
    box(width = 6, title = "Puntos de cambio de dirección",
      plotOutput('turnsplot')
    )
  ),
  fluidRow(
    box(width = 6, title = "Acumulado",
        plotOutput("cumulatedplot")
    ),
    box(width = 6, title = "Pérdidas",
        plotOutput("drawdownsdplot")
    )
  ),
  fluidRow(
    box(width = 6, title = "Boxplot Precios",
        plotOutput("pricesboxplot")
    ),
    box(width = 6, title = "Boxplot Retornos",
        plotOutput("returnsboxplot")
    )
  )
)

pairstab<-tabItem(
  tabName = "pairs",
  fluidRow(
    box(width = 6, title = "Escoga el primer instrumento",
        uiOutput("symbollist2")
    ),
    box(width = 6, title = "Escoga el segundo instrumento",
        uiOutput("symbollist3")
    )
  ),
  fluidRow(
      valueBoxOutput("covarvalue", width = 6),
      valueBoxOutput("correlvalue", width = 6)
  ),
  fluidRow(
    box(width = 6, title = "Precios",
        plotOutput("pricesplot")
    ),
    box(width = 6, title = "Retornos",
        plotOutput("returnsplot")
    )
  ),
  fluidRow(
    box(width = 6, title = "Fuerza relativa",
        plotOutput("rsplot")
    ),
    box(width = 6, title = "Retornos",
        plotOutput("scatterreturnsplot")
    )
  )
)

body<-dashboardBody(
  tabItems(datatab, statstab,
           optimizetab, individualtab,
           pairstab)
)

# Define UI
shinyUI(
  dashboardPage(
    skin = "green", header, menu, body
  )
)

