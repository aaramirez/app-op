

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
    menuItem("Pares", tabName="pairs", icon=icon("files-o")),
    menuItem("Mercado", tabName = "market", icon=icon("building"))
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
        tableOutput('returntable')
      )
    )
  )
)

statstab<-tabItem(
  tabName = "stats",
  fluidRow(
    box(width = 12, title = "Medias",
        verbatimTextOutput("meantable")
    ),
    box(width = 12, title = "Matriz de Varianza y Covarianza",
        verbatimTextOutput("varcovartable")
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

markettab<-tabItem(
  tabName = "market",
  h1("Referencias del Mercado")
)

body<-dashboardBody(
  tabItems(datatab, statstab,
           optimizetab, individualtab,
           pairstab, markettab)
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

