library(shiny)
library(ggplot2)
library(gridlayout)
library(bslib)


ui <- navbarPage(
  title = "Network Analysis",
  selected = "Read-Me",
  collapsible = TRUE,
  theme = bslib::bs_theme(bootswatch = "lumen"),
  tabPanel(title = "Read-Me"),
  tabPanel(
    title = "Data Prep",
    navlistPanel(
      tabPanel(title = "Upload Data"),
      tabPanel(title = "Peek into Data"),
      tabPanel(title = "Map ID and Other Columns")
    )
  ),
  tabPanel(title = "Find Your Duplicates"),
  tabPanel(title = "Visualise")
)


server <- function(input, output, session) {
  
}

shinyApp(ui, server)
  

