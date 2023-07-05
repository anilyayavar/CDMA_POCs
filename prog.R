########## section 1 ------------

###
###
##

sum(x)
x <- 1:10


########## UI Function -------------
vroom::vroom("Y:\\Bharatkosh Union.csv", delim = "|", col_types = c(.default = "c") )
library(tidyverse)

read_delim(
  "Y:\\Bharatkosh Union.csv", delim = "|", col_types = c(.default = "c")
)

summary(iris)

library(tidyverse)

iris %>% 
  summarise(across(everything(), ~(nrow(iris) - n_distinct(.))/nrow(iris))) %>% 
  pivot_longer(everything(), names_to = "cols", values_to = "dups") %>% 
  ggplot(aes(fct_reorder(cols, dups), dups)) +
  geom_col() +
  coord_flip()

str(iris)


## Add page for data pre-processing
## add ID column if not present there.


data <- read_csv("G:\\network_data.csv")

data %>% 
  summarise(across(everything(), ~ sum(is.na(.)))) %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(name, value)) +
  geom_col()

data %>% 
  summarise(across(everything(), ~ sum(duplicated(.)))) %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(fct_reorder(name, value), value)) +
  geom_col() +
  coord_flip()

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
  labs(x = '')


factor(data2$name, levels = unique(data2$name))


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
    labs(title = "Dupes",x = '', y = "Dupes", subtitle = "If you aren't seeing any red color, it means there are no Duplicate values in the data")
}

dup_chart(data)

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
    labs(title = "Missing values",x = '', y = 'NA Values', subtitle = "If you aren't seeing any red color, it means there are no NA values in the data") 
    
}

na_chart(data)


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

eligible_id_cols(data)


install.packages('qpdf')
library(qpdf)

qpdf::pdf_combine(c('Y:\\1.pdf', 'Y:\\2.pdf', 'Y:\\3.pdf'),
                  output = 'Y:\\out.pdf')


group_ext <- function(data){
  data %>% 
    mutate(across(everything(), as.character)) %>% 
    pivot_longer(-IE_code) %>% 
    select(-name) %>% 
    graph_from_data_frame() %>% 
    components() %>% 
    pluck(membership) %>% 
    unique()
}

graph_data <- function(data){
  data %>% 
    mutate(across(everything(), as.character)) %>% 
    pivot_longer(-IE_code) %>% 
    select(-name) %>% 
    graph_from_data_frame()
}

visIgraph(graph_data(data))

g <- graph_data(data)



visIgraph(g)



components(g)

group_ext(data)

data_summ(data)

g1 <- make_graph("Zachary")
visIgraph(g1)



make_sub(g, 1)

# Create an example graph
g <- make_graph("Zachary")
components <- clusters(g)$membership

# Create the visIgraph plot
plot <- visIgraph(g)

# Add a custom event to display membership as tooltip
tooltip_code <- '
  function(event) {
    var node = this.getNodeAt(event.pointer.DOM);
    if (node !== null) {
      var membership = this.body.data.nodes._data[node].membership;
      return "Membership: " + membership;
    }
  }
'
plot <- visIgraph(g) %>%
  visOptions(tooltip = TRUE) %>%
  visEvents(hoverNode = tooltip_code)

# Print the plot
plot


shiny::runApp(system.file("shiny", package = "visNetwork"))


library(igraph)
library(visNetwork)

# Create an example graph
g <- make_graph("Zachary")
components <- clusters(g)$membership

# Create the visIgraph plot
plot <- visIgraph(g)

# Customize tooltips with membership information
for (i in 1:vcount(g)) {
  plot <- visNodes(plot, id = i, title = paste("Membership: ", components[i]), hover = list(title = ""))
}

# Print the plot
plot

# Customize tooltips with membership information
tooltip_code <- '
  function(node) {
    return "Membership: " + node.membership;
  }
'
plot <- visOptions(plot, tooltip = list(enabled = TRUE, formatter = htmlwidgets::JS(tooltip_code)))

# Print the plot
plot

library(igraph)
library(visNetwork)

# Create an example graph
g <- make_graph("Zachary")
components <- clusters(g)$membership

# Create the visIgraph plot
plot <- visIgraph(g)

# Customize tooltips with membership information
tooltip_code <- '
  function(data) {
    var node = data.node;
    var membership = node.membership;
    var tooltip = "Membership: " + membership;
    data.title = tooltip;
    return data;
  }
'

proxy <- visNetworkProxy(plot)
visNetworkProxyInvoke(proxy, "updateOptions", options = list(tooltip = list(enabled = TRUE)))
visNetworkProxyInvoke(proxy, "updateNodes", nodes = list(title = htmlwidgets::JS(tooltip_code)))

# Print the plot
plot

visNetwork(V(g), E(g), width = '100%')
