library(shiny)
library(shinyjs)


ui <- fluidPage(
  shinyjs::useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      actionButton("Button1", "Run"),
      shinyjs::hidden(p(id = "text1", "Processing..."))
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  
  plotReady <- reactiveValues(ok = FALSE)
  
  observeEvent(input$Button1, {
    shinyjs::disable("Button1")
    shinyjs::show("text1")
    plotReady$ok <- FALSE
    # do some cool and complex stuff
    Sys.sleep(2)
    plotReady$ok <- TRUE
  })  
  
  output$plot <-renderPlot({
    if (plotReady$ok) {
      shinyjs::enable("Button1")
      shinyjs::hide("text1")
      hist(rnorm(100, 4, 1),breaks = 50)
    }
  })
}

shinyApp(ui, server)