### Load libraries ----------------
library(shiny)
library(gridlayout)
library(bslib)
library(vroom)
library(tidyverse)
library(igraph)
library(shinyjs)
library(visNetwork)
library(stringi)

### Dummy data ----------------
PANs <- paste0('PANNO', c('0000', '0000', '1111', '2222', '3333', '4444', '5555', '5555','6666', '7777'), 
               c('A', 'A', 'B', 'C', 'D', 'E', 'F', 'F', 'G', 'H'))

emails <- paste0(c('aaaa', 'bbbb', 'cccc', 'dddd', 'eeee', 'ffff', 'gggg', 'hhhh', 'iiii', 'bbbb'), '@gmail.com')


tele <- paste0('9', c('1', '2', '3', '4', '1', '1', '5', '6', '3', '2') %>% 
                 map_chr(~ rep(.x, 9) %>% 
                           paste0(collapse = '')))

dummy_data <- data.frame(
  ID = 1:10,
  Mobile = tele,
  Email = emails,
  PAN = PANs
)

dummy_gr <- dummy_data %>% 
  pivot_longer(-ID, names_to = NULL) %>% 
  graph_from_data_frame()

V(dummy_gr)$title <- names(V(dummy_gr))
V(dummy_gr)$group <-  c('B', 'A')[(names(V(dummy_gr)) %in% dummy_data$ID)+1]  

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
         subtitle = "If you aren't seeing any red color, 
         it means there are no Duplicate values in the data") +
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
    mutate(ID_col = paste0("Row_", row_number())) %>% 
    select(ID_col, everything())
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
  # Create tooltip
  V(g)$title <- paste('Group ID: ', clusters(g)$membership)
  # Create Colors
  colors <- g %>% 
    components() %>% 
    pluck(membership) %>% 
    max() %>% 
    rainbow()
  # Assign Colors
  V(g)$color <- colors[components(g)$membership]
  # create Viz
  visIgraph(g, layout = 'layout_with_fr')
}

make_sub <- function(g, mem, id_col){
  # Identify clusters
  components <- clusters(g)$membership
  
  # induced subgraph
  g1 <- induced_subgraph(g, which(components == mem))
  
  # Allocate Group
  V(g1)$group <-  c('B', 'A')[(names(V(g1)) %in% id_col)+1]
  
  # make induced Subgraph
   visIgraph(g1, layout = 'layout_with_fr') %>% 
    visGroups(groupname = "A", shape = 'icon',
              icon = list(code = 'f007', size = 75, color = 'red')) %>% 
    visGroups(groupname = "B", shape = 'icon',
              icon = list(code = 'f2bc', size = 75, color = 'seagreen')) %>% 
    addFontAwesome()
}

### UI -----------------
ui <- navbarPage(
  title = "Network Analysis",
  selected = "Read-Me",
  collapsible = TRUE,
  theme = bslib::bs_theme(bootswatch = "lumen"),
  shinyjs::useShinyjs(),
  tabPanel(title = "Read-Me",
           fluidRow(
             column(6,
                    p("Detection of duplicates is a frequent requirement in audit analytics, 
                      be it case of detecting risk of duplicate invoices or simply detection 
             of duplicates in social sector audits."),
             # br(),
             p('Detection of duplicates among selected records, is however, not difficult.'),
             # br(),
             p('However, during social sector audits, etc. 
               a need may arise to find duplicate records of beneficiaries/entities who may have created
             multiple identities on the basis of giving different id proofs against each record.'),
             p('Imagine a scenario where benefits of a DBT scheme is allowed to beneficiaries only once.  
               However, a set of identity records
             are required e.g. Aadhaar number/Token, PAN number, Bank Account Number, Mobile, Email, etc.'),
             p('See the data - adjoining.')
                                 ),
             column(6, 
                    tableOutput('dummy_dat'))
           ),
           fluidRow(column(12, hr())),
           fluidRow(
             column(6, 
                    p('In the above we can see that there are four duplicates on the basis of Mobile, 
                      two duplicates in Email and two duplicates
             on the basis of PAN numbers.'),
             p('If we draw a network chart of these 10 beneficiaries 
               we can find there are only 3 unique beneficiaries connected
             with different attributes.  See the adjoining plot.  
             The plot is interactive and you can zoom in and out to see the elements clearly.'),
             p('In the plot above')
                    ),
             column(6, 
                    visNetwork::visNetworkOutput('plot_dummy')
                    )
           )
           ),
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
                                          p(em("View First Five columns and Six rows of new data"), align = 'center'),
                                          hr(),
                                          tableOutput("head_id")#,
                                          #verbatimTextOutput('dummy')
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
                                                                                  placeholder = 'Select upto a 
                                                                                  max of four columns')
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
  
  # display dummy data
  output$dummy_dat <- renderTable(dummy_data)
  output$plot_dummy <- visNetwork::renderVisNetwork(
    visIgraph(dummy_gr, layout = 'layout_with_fr') %>% 
      visGroups(groupname = "A", shape = 'icon',
                icon = list(code = 'f007', size = 75, color = 'red')) %>% 
      visGroups(groupname = "B", shape = 'icon',
                icon = list(code = 'f2bc', size = 75, color = 'seagreen')) %>% 
      addFontAwesome()
  )
  
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
  
  ## Create Reactive value
  data_change <- reactiveValues(ok = FALSE)
  
  observeEvent(input$button_id, {
    data_change$ok <- TRUE
  })
  
  
  # Create alternative data
  data1 <- eventReactive(input$button_id, {
    data_id(data())
  })
  
  
  output$head_id <- renderTable(head_5(data1()))
  
  
  ## select input ID values to be updated - reactive var
  id_cols <- reactive({
    if(data_change$ok){
      eligible_id_cols(data1())
    } else {
      eligible_id_cols(data())
    }
  })
  
  #output$dummy <- renderPrint(id_cols())
  
  # create the select input based on the numeric columns in the dataframe
  observeEvent(c(input$file, input$delims, input$button_id), {
    req(data())
    #num_cols <- data()
    updateSelectInput(session, 
                      "variable", 
                      choices = id_cols()
    )
    
  })
  
  # update other values to be selected based on ID column selection
  observeEvent(input$variable, {
    
    if(data_change$ok){
      other_cols <- setdiff(colnames(data1()), 
                            input$variable)
    } else {
      other_cols <- setdiff(colnames(data()), 
                            input$variable)
    }
    
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
    if(data_change$ok){
      data1()[, c(input$variable, 
                 input$other_vars)]
    } else {
      data()[, c(input$variable, 
                 input$other_vars)]
    }
    
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
    if(plotReady3$ok){
      make_sub(data_g(), as.integer(input$spec_plot), data2()[ ,input$variable])
    }
  )
  
}

### run app ---------

shinyApp(ui, server)


