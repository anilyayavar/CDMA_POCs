library(shiny)
library(gridlayout)
library(bslib)
library(vroom)
library(tidyverse)
library(igraph)
library(shinyjs)

### Helper Functions ----------------
dup_chart <- function(data) {
  rbind(
    data %>% 
      summarise(across(everything(), ~n())) %>% 
      mutate(col = 'N'),
    data %>% 
      summarise(across(everything(), ~ sum(duplicated(.)))) %>% 
      mutate(col = 'Dupes'),
    data %>% 
      summarise(across(everything(), ~ sum(is.na(.)))) %>% 
      mutate(col = 'NAs')
  ) %>% 
    pivot_longer(cols = -col) -> data2
  
  data2 %>% 
    filter(col == 'N') %>% 
    ggplot() +
    geom_col(aes(factor(name, levels = rev(unique(data2$name))), value), fill = 'grey') +
    geom_col(data = filter(data2, col == 'Dupes'), aes(name, value), fill = 'red') +
    coord_flip() +
    labs(title = "Dupes",x = '', y = "Dupes", subtitle = "If you aren't seeing any red color, it means there are no Duplicate values in the data") +
    theme_minimal()
}

na_chart <- function(data) {
  rbind(
    data %>% 
      summarise(across(everything(), ~n())) %>% 
      mutate(col = 'N'),
    data %>% 
      summarise(across(everything(), ~ sum(duplicated(.)))) %>% 
      mutate(col = 'Dupes'),
    data %>% 
      summarise(across(everything(), ~ sum(is.na(.)))) %>% 
      mutate(col = 'NAs')
  ) %>% 
    pivot_longer(cols = -col) -> data2
  
  data2 %>% 
    filter(col == 'N') %>% 
    ggplot() +
    geom_col(aes(factor(name, levels = rev(unique(data2$name))), value), fill = 'grey') +
    geom_col(data = filter(data2, col == 'NAs'), aes(name, value), fill = 'red') +
    coord_flip() +
    labs(title = "Missing values",x = '', y = 'NA Values', subtitle = "If you aren't seeing any red color, it means there are no NA values in the data") +
    theme_minimal()
  
}

data_id <- function(data){
  data %>% 
    mutate(ID_col = paste0("Row_", row_number()))
}

eligible_id_cols <- function(data){
  intersect(data %>% 
              summarise(across(everything(), ~ sum(is.na(.)))) %>% 
              pivot_longer(everything()) %>% 
              filter(value == 0) %>% 
              pull(name),
            data %>% 
              summarise(across(everything(), ~ sum(duplicated(.)))) %>% 
              pivot_longer(everything()) %>% 
              filter(value == 0) %>% 
              pull(name))
}

### UI -----------------
ui <- navbarPage(
  title = "Network Analysis",
  selected = "Read-Me",
  collapsible = TRUE,
  theme = bslib::bs_theme(bootswatch = "lumen"),
  shinyjs::useShinyjs(),
  tabPanel(title = "Read-Me",
           "GGGG"),
  tabPanel(title = "Data Prep",
           navlistPanel(widths = c(2, 10),
                        tabPanel("Upload Data",
                                 fluidRow(
                                   column(3,
                                          wellPanel(h3("Upload your data"),
                                                    fileInput("file", 
                                                              "Upload CSV (Size: Upto max 5 mb)", 
                                                              accept = ".csv"
                                                    ),
                                                    selectInput("delims", 
                                                                "Choose Your Delimiter", 
                                                                choices = c(",", ";", " ", "~", "|"))
                                          )
                                   ),
                                   column(7, wellPanel(p(em("Top 6 Rows"), align = "center"),
                                                       hr(),
                                                       tableOutput("head")))
                                 )),
                        tabPanel("Peek into Data",
                                 fluidRow(
                                   column(3, 
                                          wellPanel(
                                            p(strong("See Missing values")),
                                            actionButton("button_na", 'See Missing value Plot'),
                                            p("Check that your ID column should not have any missing value")
                                          )
                                          ),
                                   column(7, wellPanel(
                                     p(em("Missing value Plot"), align = 'center'),
                                     hr(),
                                     plotOutput("na_plot")
                                   ))
                                 ),
                                 fluidRow(
                                   column(3, 
                                          wellPanel(
                                            p(strong("See Duplicates available in the data")),
                                            actionButton("button_dup", 'See Duplicate value Plot'),
                                            p("Check that your ID column/variable should not have any duplicate values therein"),
                                            p(strong("Note: If you do not have any column that satisfies both these criteria, 
                                                 generate a dummy ID column in the next tab/section"))
                                          )),
                                   column(7, wellPanel(
                                     p(em("Duplicate values"), align = 'center'),
                                     hr(),
                                     plotOutput("dup_plot")
                                   ))
                                 )),
                        tabPanel("Create ID Column - Optional",
                                 fluidRow(
                                   column(3, 
                                          wellPanel(
                                            actionButton("button_id", "generate Dummy ID Column")
                                          )
                                          ),
                                   column(7, 
                                          tableOutput("head_id")
                                          )
                                 )),
                        tabPanel("Map ID and Other Columns",
                                 fluidRow(
                                   column(3,
                                          wellPanel(selectInput("variable", 
                                                                "Select ID Column", 
                                                                choices = NULL
                                          )
                                          ),
                                          wellPanel(selectizeInput("other_vars", 
                                                                   "Select other attribute columns", 
                                                                   multiple = TRUE,
                                                                   choices = NULL,
                                                                   options = list(maxItems = 3, 
                                                                                  placeholder = 'Select upto a max of four columns')
                                          )
                                          )
                                   ),
                                   column(7, "7")
                                 )
                        )
           )),
  tabPanel(title = "Find Your Duplicates",
           fluidRow(
             column(3, wellPanel(downloadButton("download", "Download.csv"))),
             column(7, wellPanel(p(em("Output"), align = "center"),
                                 hr(),
                                 dataTableOutput("head_2")))
           )),
  tabPanel(title = "Visualise")
)


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
    
    # let the user know if the data contains eligible ID Column
    # validate(need(length(eligible_id_cols(dataset)) != 0,
    #               "This dataset has no numeric columns."))
    dataset
  })
  
  ## select input ID values to be updated - reactive var
  id_cols <- reactive({
    eligible_id_cols(data())
  })
  
  
  
  # create the select input based on the numeric columns in the dataframe
  observeEvent(c(input$file, input$delims), {
    req(data())
    #num_cols <- data()
    updateSelectInput(session, 
                      "variable", 
                      choices = id_cols()
    )
    
  })
  
  # update other values to be selected based on ID column selection
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
  
  # Modify data - retain selected columns only
  data2 <- reactive({
    req(input$other_vars)
    data()[, c(input$variable, 
               input$other_vars)]
    
  })
  
  # Network analysis
  # Create Group ID which is a cluster ID
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
  
  ## Reactive val
  plotReady <- reactiveValues(ok = FALSE)
  
  observeEvent(input$button_na, {
    shinyjs::disable('button_na')
    plotReady$ok <- TRUE
  })
  
  ## NA plot
  output$na_plot <- renderPlot({
    if(plotReady$ok){
      shinyjs::disable("button_na")
      na_chart(data())
    }
      
  })
  
  ## Reactive val2
  plotReady2 <- reactiveValues(ok = FALSE)
  
  observeEvent(input$button_dup, {
    shinyjs::disable('button_dup')
    plotReady2$ok <- TRUE
  })
  
  ## NA plot
  output$dup_plot <- renderPlot({
    if(plotReady2$ok){
      shinyjs::disable("button_dup")
      dup_chart(data())
    }
    
  })
  
  # View Final Data
  output$head_2 <- renderDataTable({
    data_net() 
  })
  
  # Prepare data for download
  output$download <- downloadHandler(
    filename = function() {
      paste0(input$file, ".csv")
    },
    content = function(file) {
      vroom::vroom_write(data_net(), file, delim = ",")
    }
  )
  
}

shinyApp(ui, server)


