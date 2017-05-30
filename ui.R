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

header<-dashboardHeader(title = "Optimización de Portafolios", titleWidth = "300px")

menu<-dashboardSidebar(sidebarMenu(
  menuItem("Datos", tabName = "data", icon=icon("table")),
  menuItem("Estadísticas", tabName = "stats", icon=icon("pie-chart")),
  menuItem("Optimización", tabName = "optimize", icon=icon("area-chart")),
  menuItem("Instrumento", tabName="individual", icon=icon("file-o")),
  menuItem("Pares", tabName="pairs", icon=icon("files-o")),
  menuItem("Mercado", tabName = "market", icon=icon("building"))
))

body<-dashboardBody(
  tabItems(
    tabItem(tabName = "data",
            h1("Carga de datos")
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
