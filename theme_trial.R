library(shiny)
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Application Title"),
  navlistPanel(
    # adjusts the width of the navigation panel. The grid is 12 columns wide.
    widths = c(3, 9),
    tabPanel("Directions",
             h4("here is some text"),
             p("more text."),
             br(),
             HTML("<ol><li>Mean absolute error by participants</li><li>Absolute error for each observation</li><li>Participant plots</li></ol>"),
             br(),
             p("Each of the",  strong("mean absolute"), "and", strong("absolute error"), "plots have an input box that allows you to", strong("filter out participants or observations below a certain threshold."), " plots!"),
             p("In the", strong("mean absolute and absolute error plots"), ", the", span("Participant plots", style = "color:#337ab7")," and see the observed and predicted value for the absolute error plots.")),
    tabPanel("Title 1",
             fluidRow(column(12,
                             wellPanel(
                               selectInput("mod_mae",
                                           "Select a model:", c("OLS" = "OLS",
                                                                "Bayes - All" = "All",
                                                                "Bayes - S10" = "Sigma 10")),
                               numericInput("mae10", label = "Suppress subjects with MAE less than:", 10, min = 0)))),
             fluidRow(column(12,
                             plotOutput("Plot")))), 
    tabPanel("Title 2",
             fluidRow(column(12,
                             wellPanel(
                               selectInput("mod",
                                           "Select a model:", c("OLS" = "OLS",
                                                                "Bayes - All" = "All",
                                                                "Bayes - S10" = "Sigma 10")),
                               numericInput("ae10", label = "Suppress observations with AE less than:", 10, min = 0)))),
             fluidRow(column(12,
                             plotOutput("Plot2")))))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$Plot <- renderPlot({
    plot(x <- sort(rnorm(47)), type = "s", main = "plot(x, type = \"s\")")
    points(x, cex = .5, col = "dark red")
  })
  
  output$Plot2 <- renderPlot({
    plot(x <- sort(rnorm(47)), type = "s", main = "plot(x, type = \"s\")")
    points(x, cex = .5, col = "dark red")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)