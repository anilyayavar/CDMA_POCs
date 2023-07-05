####### Add Libraries ----------------------
library(shiny)
library(tidyverse)
library(igraph)

##### Helper Functions and variables ----------------




############### UI function -----------------

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "lumen"),
  titlePanel(
    h1("Your One stop solution to find a network of duplicates")
  ),
  sidebarLayout(
    sidebarPanel(
      wellPanel(h3("Upload your data"),
                fileInput("file", 
                          "Upload CSV (Size: Upto max 5 mb)", 
                          accept = ".csv"
                ),
                selectInput("delims", "Choose Your Delimiter", choices = c(",", ";", " ", "~", "|"))
      ),
      br(),# file widget
      wellPanel(selectInput("variable", 
                            "Select ID Column", 
                            choices = NULL
      )
      ),
      br(), # select widget
      wellPanel(selectizeInput("other_vars", 
                               "Select other attribute columns", 
                               multiple = TRUE,
                               choices = NULL,
                               options = list(maxItems = 3, 
                                              placeholder = 'Select upto a max of four columns')
                               )
                ), # other selections
      #actionButton("calc", "Calculate! and display top 10 results"),
      br(),
      wellPanel(downloadButton("download", "Download.csv"))
    ),
    mainPanel(
      p("Made by - Anil Goyal & Chandersheel", align = "right"),
      hr(),
      br(),
      wellPanel(p(em("Top 6 Rows"), align = "center"),
                hr(),
                tableOutput("head")),
      br(),
      wellPanel(p(em("Output"), align = "center"),
                hr(),
                dataTableOutput("head_2"))
    )
  )
)

######### Server Function ----------------
server <- function(input, output,session) {
  
  # Solve delim issue
  # delims <- reactive({
  #   input$delims
  # })

  
  # get data from file
  data <- reactive({
    req(input$file)
    
    # as shown in the book, lets make sure the uploaded file is a csv
    ext <- tools::file_ext(input$file$name)
    validate(need(ext == "csv", 
                  "Invalid file format. Please upload a .csv file only"
    )
    )
    file1 <- input$file
    if(is.null(file1)){return()}
    dataset <- vroom::vroom(
      input$file$datapath, 
      delim = input$delims,
      col_types = c(.default = "c")
    )
    
    # let the user know if the data contains no numeric column
    # validate(need(ncol(dplyr::select_if(dataset, is.numeric)) != 0,
    #               "This dataset has no numeric columns."))
    dataset
  })
  
  # create the select input based on the numeric columns in the dataframe
  observeEvent(c(input$file, input$delims), {
    req(data())
    #num_cols <- data()
    updateSelectInput(session, 
                      "variable", 
                      choices = colnames(data())
    )
    
  })
  
  observeEvent(input$variable, {
    #req()
    other_cols <- setdiff(colnames(data()), 
                          input$variable)
    updateSelectizeInput(session, 
                         "other_vars", 
                         choices = other_cols)
  })
  
  # View First 6 Rows
  output$head <- renderTable({
    data() %>% 
      mutate(across(everything(), 
                    as.character)) %>% 
      head(6)
  })
  
  data2 <- reactive({
    req(input$other_vars)
    data()[, c(input$variable, 
               input$other_vars)] %>% 
      mutate(across(everything(), 
                    as.character))
    
  })
  
  data_net <- reactive({
    data2() %>% 
      pivot_longer(cols = all_of(input$other_vars),
                   values_drop_na = TRUE) %>% 
      select(all_of(input$variable), value) %>% 
      graph_from_data_frame() %>% 
      components() %>% 
      pluck(membership) %>% 
      stack() %>% 
      set_names(c('GROUP_ID', input$variable)) %>% 
      as.tibble() %>% 
      right_join(data2() %>% 
                   mutate(across(.cols = all_of(input$variable), as.factor)),
                 by = (input$variable)) %>% 
      mutate(GROUP_ID = as.integer(GROUP_ID)) %>% 
      arrange(GROUP_ID, list(input$other_vars))
  })
  
  # View Final Data
  output$head_2 <- renderDataTable({
    data_net() 
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste0(input$file, ".csv")
    },
    content = function(file) {
      vroom::vroom_write(data_net(), file, delim = ",")
    }
  )
  
}

#### Run app -------------- 
shinyApp(ui, server)