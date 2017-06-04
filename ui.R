

# Make sure a package is at least some version (only installs from CRAN)
ensure_version <- function(pkg, ver = "0.0") {
  if (system.file(package = pkg)  == "" || packageVersion(pkg) < ver)
    install.packages(pkg)
}

ensure_version("shiny", "1.0.0")
ensure_version("shinydashboard", "0.5.3")

library(shiny)
library(shinydashboard)

source("text.R")

header<-dashboardHeader(
  title = tagList(icon("briefcase"), APPTITLE),
  titleWidth = "300px")

menu<-dashboardSidebar(
  sidebarMenu(
    id = "menuitems",
    menuItem("Datos", tabName = "data", icon=icon("table"), selected = TRUE),
    menuItem("Estadísticas", tabName = "stats", icon=icon("pie-chart")),
    menuItem("Optimización", tabName = "optimize", icon=icon("area-chart")),
    menuItem("Instrumento", tabName="individual", icon=icon("file-o")),
    menuItem("Pares", tabName="pairs", icon=icon("files-o"))
  )
)

datatab<-tabItem(
  tabName = "data",
  fluidRow(
    box(width = 3, title = "Cargar lista de símbolos",
        fileInput('file_w_data', 'Seleccione el archivo de símbolos',
                  accept = c(
                    'text/csv',
                    'text/comma-separated-values',
                    'text/tab-separated-values',
                    'text/plain',
                    '.csv',
                    '.tsv'
                  )
        ),
        tags$hr(),
        checkboxInput('header', 'Con Encabezado', TRUE),
        radioButtons('sep', 'Separador',
                     c('Coma'=',',
                       'Punto y coma'=';',
                       'Tab'='\t'),
                     ','),
        radioButtons('quote', 'Comillas',
                     c('Ninguna'='',
                       'Comilla doble'='"',
                       'Comilla simple'="'"),
                     ''),
        tags$hr(),
        #TODO: Colocar el archivo de prueba en la nube y arreglar el URL
        p('Si desea puede trabajar con un archivo de ejemplo .csv,',
          'primero descargue el archivo',
          a(href = 'smallcap.csv', 'smallcap.csv'),
          'y luego utilicelo.'
        )
    ),
    tabBox(
      width = 9, title = "Símbolos y Retornos",
      tabPanel(
        icon = icon("check-circle"), title = "Lista de símbolos",
        tableOutput('datatable')
      ),
      tabPanel(
        icon = icon("check-circle"),title = "Retorno de los datos",
        fluidRow(
          box(radioButtons(inputId = "returnsType",
                           label = "Tipo de retorno",
                           choices = list("Aritmético"="arithmetic", "Logarítmico"="log")), width = 12)
        ),
        tableOutput('returntable')
      ),
      tabPanel(
        icon = icon("check-circle"), title = "Retorno acumulado",
        tableOutput('cumulatedtable')
      ),
      tabPanel(
        icon = icon("check-circle"), title = "Pérdidas",
        tableOutput('drawdowstable')
      )
    )
  )
)

statstab<-tabItem(
  tabName = "stats",
  fluidRow(
    tabBox(
      width = 12, title = "Estadísticas generales",
      tabPanel(
        icon = icon("check-circle"), title = "Resumen de precios y retornos",
        h2("Resumen de precios"),
        verbatimTextOutput("summarypricestext"),
        h2("Resumen de retornos"),
        verbatimTextOutput("summaryreturnstext")
      ),
      tabPanel(
        icon = icon("check-circle"), title = "Estadísticas básicas",
        h2("Estadísticas de los precios"),
        verbatimTextOutput("basicstatspricestext"),
        h2("Estadísticas de los retornos"),
        verbatimTextOutput("basicstatsreturnstext")
      ),
      tabPanel(
        icon = icon("check-circle"), title = "Matriz de Varianza y Covarianza",
        verbatimTextOutput("meantable"),
        verbatimTextOutput("varcovartable")
      ),
      tabPanel(
        icon = icon("check-circle"), title = "Períodos de pérdidas",
        fluidRow(
          box(width = 12, title = "Seleccione el instrumento que desea estudiar:",
              uiOutput("symbollist4")
          )
        ),
        fluidRow(
          box(width = 12, title = "Períodos de pérdida del instrumento",
              tableOutput("drawdownstable")
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
    box(width = 6, title = "Kernel",
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
    skin = "green",
    header,
    menu,
    body
  )
)

