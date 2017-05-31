#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Make sure a package is at least some version (only installs from CRAN)
ensure_version <- function(pkg, ver = "0.0") {
  if (system.file(package = pkg)  == "" || packageVersion(pkg) < ver)
    install.packages(pkg)
}

ensure_version("shiny", "1.0.0")
ensure_version("shinydashboard", "0.5.3")

library(shiny)
library(shinydashboard)

header<-dashboardHeader(
  title = tagList(shiny::icon("briefcase"), "Optimización de Portafolios"),
  titleWidth = "300px")

menu<-dashboardSidebar(sidebarMenu(id = "menuitems",
  menuItem("Datos", tabName = "data", icon=icon("table"), selected = TRUE),
  menuItem("Estadísticas", tabName = "stats", icon=icon("pie-chart")),
  menuItem("Optimización", tabName = "optimize", icon=icon("area-chart")),
  menuItem("Instrumento", tabName="individual", icon=icon("file-o")),
  menuItem("Pares", tabName="pairs", icon=icon("files-o")),
  menuItem("Mercado", tabName = "market", icon=icon("building"))
))

body<-dashboardBody(
  tabItems(
    tabItem(tabName = "data",
        fluidRow(
          box(width = 3, title = "Cargar lista de símbolos",
            fileInput('file_w_data', 'Choose file with symbols to upload',
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
            checkboxInput('header', 'Header', TRUE),
            radioButtons('sep', 'Separator',
                         c(Comma=',',
                           Semicolon=';',
                           Tab='\t'),
                         ','),
            radioButtons('quote', 'Quote',
                         c(None='',
                           'Double Quote'='"',
                           'Single Quote'="'"),
                         '"'),
            tags$hr(),
            p('If you want a sample .csv or .tsv file to upload,',
              'you can first download the sample',
              a(href = 'mtcars.csv', 'mtcars.csv'), 'or',
              a(href = 'pressure.tsv', 'pressure.tsv'),
              'files, and then try uploading them.'
            )
          ),
          tabBox(width = 9, title = "Símbolos y Retornos",
            tabPanel(icon = icon("check-circle"), title = "Lista de símbolos",
                  tableOutput('datatable')
            ),
            tabPanel(icon = icon("check-circle"),title = "Retorno de los datos",
                   tableOutput('returntable')
            )
          )
        )
    ),
    tabItem(tabName = "stats",
            h1("Estadísticas del Portafolio")
    ),
    tabItem(tabName = "optimize",
            h1("Optimización del Portafolio")
    ),
    tabItem(tabName = "individual",
            h1("Datos del Instrumento")
    ),
    tabItem(tabName = "pairs",
            h1("Comparación de instrumentos")
    ),
    tabItem(tabName = "market",
            h1("Referencias del Mercado")
    )
  )
)

# Define UI
shinyUI(dashboardPage(skin = "green",
  header,
  menu,
  body
))
