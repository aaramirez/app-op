

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

menu<-dashboardSidebar(sidebarMenu(id = "menuitems",
                                   menuItem("Datos", tabName = "data", icon=icon("table"), selected = TRUE),
                                   menuItem("Estadísticas", tabName = "stats", icon=icon("pie-chart")),
                                   menuItem("Optimización", tabName = "optimize", icon=icon("area-chart")),
                                   menuItem("Instrumento", tabName="individual", icon=icon("file-o")),
                                   menuItem("Pares", tabName="pairs", icon=icon("files-o")),
                                   menuItem("Mercado", tabName = "market", icon=icon("building"))
))

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
                     '"'),
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
  h1("Estadísticas del Portafolio")
)

optimizetab<-tabItem(
  tabName = "optimize",
  h1("Optimización del Portafolio")
)

individualtab<-tabItem(
  tabName = "individual",
  h1("Datos del Instrumento")
)

pairstab<-tabItem(
  tabName = "pairs",
  h1("Comparación de instrumentos")
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

