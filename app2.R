### Load libraries ----------------
library(shiny)
library(gridlayout)
library(bslib)
library(vroom)
library(tidyverse)
library(igraph)
library(shinyjs)
library(visNetwork)

### Helper Functions ----------------

data_summ <- function(data){
  cat(paste0('The data contains\n - ', 
             nrow(data),
             ' rows and\n - ', 
             ncol(data), 
             ' columns.\n',
             'If there is mismatch between row and/or column counts, check the de-limiter'
  ))
}


head_5 <- function(data){
  data %>% 
    select(seq(min(5, ncol(data)))) %>% 
    head(6)
}

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
    labs(title = "Dupes",
         x = '', 
         y = "Dupes", 
         subtitle = "If you aren't seeing any red color, it means there are no Duplicate values in the data") +
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
    geom_col(data = filter(data2, col == 'NAs'), 
             aes(name, value), fill = 'red') +
    coord_flip() +
    labs(title = "Missing values",
         x = '', 
         y = 'NA Values', 
         subtitle = "If you aren't seeing any red color, it means there are no NA values in the data") +
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

group_ext <- function(data){
  data %>% 
    components() %>% 
    pluck(membership) %>% 
    unique()
}

my_viz <- function(g){
  V(g)$title <- paste('Group ID: ', clusters(g)$membership)
  visIgraph(g, layout = 'layout_with_fr')
}

make_sub <- function(g, mem){
  components <- clusters(g)$membership
  induced_subgraph(g, which(components == mem)) %>% 
    visIgraph(layout = 'layout_with_fr')
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
                                   column(7, 
                                          fluidRow(wellPanel(
                                            verbatimTextOutput("summ")
                                          )
                                          ),
                                          fluidRow(wellPanel(p(em("Top 6 Rows - At most first 5 Columns will only be displayed"), 
                                                               align = "center"),
                                                             hr(),
                                                             tableOutput("head")))
                                          )
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
             column(3, 
                    wellPanel(actionButton("button_calc", "Calculate Network of Duplicates")),
                    wellPanel(downloadButton("download", "Download.csv"))
                    ),
             column(7, wellPanel(p(em("Output"), align = "center"),
                                 hr(),
                                 dataTableOutput("head_2")))
           )),
  tabPanel(title = "Visualise",
           fluidRow(
             column(3, wellPanel(
               actionButton('gen_plot', "Generate Plot")
             )),
             column(9, 
                    visNetwork::visNetworkOutput('plot1'))
           ),
           fluidRow(
             column(3, wellPanel(
               selectInput('spec_plot', 'Select Group ID/Cluster ID to visualise any single Cluster', choices = NULL)
             )),
             column(9, 
                    visNetwork::visNetworkOutput('plot2')
                    )
           )
           )
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
  
  ## Present data Summary
  output$summ <- renderPrint({
    data_summ(data())
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
    head_5(data())
  })
  
  # Modify data - retain selected columns only
  data2 <- reactive({
    req(input$other_vars)
    data()[, c(input$variable, 
               input$other_vars)]
    
  })
  
  # Network analysis
  # Create graph object
  data_g <- reactive({
    data2() %>% 
      pivot_longer(cols = all_of(input$other_vars),
                   values_drop_na = TRUE) %>% 
      select(all_of(input$variable), value) %>% 
      graph_from_data_frame()
  })
  
  
  # Create Group ID which is a cluster ID
  data_net <- reactive({
    data_g() %>% 
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
    #shinyjs::disable('button_na')
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
  
  ## Another Reactive Val
  calcReady3 <- reactiveValues(ok = FALSE)
  
  observe(if(!calcReady3$ok){
    shinyjs::disable("button_calc")
    shinyjs::disable('gen_plot')
    shinyjs::disable('download')
  })
  # 
  observeEvent(input$other_vars, {
    shinyjs::enable('button_calc')
  })
  
  observeEvent(input$button_dup, {
    shinyjs::disable('button_dup')
    plotReady2$ok <- TRUE
  })
  
  # Create Reactive Value for generating plot
  plotReady3 <- reactiveValues(ok = FALSE)
  
  # calculate fresh choices for group ID
  
  groups <- reactive({
    group_ext(data_g())
  })
  
  
  # Enable/Disable reactive Value 3
  observeEvent(input$button_calc, {
    shinyjs::enable('gen_plot')
    shinyjs::enable('download')
    calcReady3$ok <- TRUE
    updateSelectInput(session, 'spec_plot',
                      choices = groups(), 
                      selected = NULL)
  })
  
  
  
  observeEvent(input$gen_plot, {
    shinyjs::enable('gen_plot')
    plotReady3$ok <- TRUE
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
    if(calcReady3$ok){
      data_net()
    } 
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
  
  # View Plot
  output$plot1 <- visNetwork::renderVisNetwork(
    if(plotReady3$ok){
      shinyjs::disable("button_calc")
      #shinyjs::disable('gen_plot')
      my_viz(data_g())
    }
  )
  
  output$plot2 <- visNetwork::renderVisNetwork(
    make_sub(data_g(), input$spec_plot)
  )
  
}

### run app ---------

shinyApp(ui, server)


