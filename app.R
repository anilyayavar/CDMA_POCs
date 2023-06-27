####### Add Libraries ----------------------
library(shiny)
library(tidyr)
library(dplyr)
library(igraph)

############### UI function -----------------

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "united"),
  titlePanel(
    h1("Your One stop solution to find a network of duplicates")
  ),
  sidebarLayout(
    sidebarPanel(
      h3("Upload your data"),
      fileInput("file", 
                "Upload CSV (Size: Upto max 5 mb)", 
                accept = ".csv"
      ), # file widget
      selectInput("variable", 
                  "Select ID Column", 
                  choices = NULL
      ), # select widget
      selectizeInput("other_vars", 
                     "Select other attribute columns", 
                     multiple = TRUE,
                     choices = NULL,
                     options = list(maxItems = 3, 
                                    placeholder = 'Select upto a max of four columns')
      ), # other selections
      downloadButton("download", "Download.csv")
    ),
    mainPanel(
      h6("Made by - Anil Goyal & Chandersheel"),
      tableOutput("head"),
      tableOutput("head_2")
    )
  )
)

######### Server Function ----------------
server <- function(input, output,session) {
  
  # get data from file
  data <- reactive({
    req(input$file)
    
    # as shown in the book, lets make sure the uploaded file is a csv
    ext <- tools::file_ext(input$file$name)
    validate(need(ext == "csv", 
                  "Invalid file format. Please upload a .csv file only"
    )
    )
    
    dataset <- vroom::vroom(
      input$file$datapath, 
      delim = ","
    )
    
    # let the user know if the data contains no numeric column
    # validate(need(ncol(dplyr::select_if(dataset, is.numeric)) != 0,
    #               "This dataset has no numeric columns."))
    dataset
  })
  
  # create the select input based on the numeric columns in the dataframe
  observeEvent(input$file, {
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
                 by = all_of(input$variable)) %>% 
      mutate(GROUP_ID = as.integer(GROUP_ID)) %>% 
      arrange(GROUP_ID, list(input$other_vars))
  })
  
  # Another View 6 rows
  output$head_2 <- renderTable({
    data_net() %>% 
      head(10)
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

#### Run app 
shinyApp(ui, server)