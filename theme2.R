library(shiny)

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Demo"),
    tabPanel("Prep")
  )
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)

runExample("06_tabsets")

?runExample

?switch
install.packages('shinyuieditor')
remotes::install_github("rstudio/shinyuieditor")
library(shinyuieditor)
shinyuieditor::launch_editor(app_loc = "new-app/")
