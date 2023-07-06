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

data3 <- data %>% 
  mutate(across(everything(), as.character)) %>% 
  mutate(ID = row_number(),
         telephone = stri_reverse(telephone),
         telephone = paste0('9', str_sub(telephone, 1, 9)),
         PAN = PANs,
         email = emails) %>% 
  select(ID, everything()) %>% 
  select(-IE_code)

data3$telephone %>% duplicated()


PANs <- paste0('PANNO', c('0000', '0000', '1111', '2222', '3333', '4444', '5555', '5555','6666', '7777'), 
       c('A', 'A', 'B', 'C', 'D', 'E', 'F', 'F', 'G', 'H'))

emails <- paste0(c('aaaa', 'bbbb', 'cccc', 'dddd', 'eeee', 'ffff', 'gggg', 'hhhh', 'iiii', 'bbbb'), '@gmail.com')


tele <- paste0('9', c('1', '2', '3', '4', '1', '1', '5', '6', '3', '2') %>% 
                 map_chr(~ rep(.x, 9) %>% 
                           paste0(collapse = '')))

 
V(dummy_gr)

dummy_data %>% 
  pivot_longer(-ID, names_to = 'title') %>% 
  select(ID, value, title) %>% 
  graph_from_data_frame() %>% 
  visIgraph()


V(dummy_gr)$group <-  c('B', 'A')[(names(V(dummy_gr)) %in% dummy_data$ID)+1]


  
visIgraph(dummy_gr) %>% 
  visGroups(groupname = "A", shape = 'icon',
            icon = list(code = 'f007', size = 75, color = 'red')) %>% 
  visGroups(groupname = "B", shape = 'icon',
            icon = list(code = 'f2bc', size = 75, color = 'seagreen')) %>% 
  addFontAwesome()


make_sub <- function(g, mem, id_col){
  # Identify clusters
  components <- clusters(g)$membership
  
  # Allocate Group
  V(g)$group <-  c('B', 'A')[(names(V(g)) %in% id_col)+1]
  
  # make induced Subgraph
  induced_subgraph(g, which(components == mem)) %>% 
    visIgraph(layout = 'layout_with_fr') %>% 
    visGroups(groupname = "A", shape = 'icon',
              icon = list(code = 'f007', size = 75, color = 'red')) %>% 
    visGroups(groupname = "B", shape = 'icon',
              icon = list(code = 'f2bc', size = 75, color = 'seagreen')) %>% 
    addFontAwesome()
}

make_sub(dummy_gr, 1, dummy_data[,'ID'])


library(visNetwork)
library(igraph)


colors <- dummy_gr %>% 
  components() %>% 
  pluck(membership) %>% 
  max() %>% 
  rainbow()

V(dummy_gr)$color <- colors[components(dummy_gr)$membership]

visIgraph(dummy_gr, layout = 'layout_with_fr')


################# ------
df <- structure(list(row_id = c("1", "2", "3", "4", "5", "6", "7", 
                                "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", 
                                "19", "20"), patient_id = c("1", "1", "1", "1", "1", "2", "2", 
                                                            "2", "1", "3", "3", "3", "4", "4", "5", "5", "6", "7", "8", "8"
                                ), pack_id = c("12", "62", "77", "86", "20", "55", "86", "72", 
                                               "7", "54", "75", "26", "21", "12", "49", "35", "51", "31", "10", 
                                               "54"), hosp_id = c("1", "2", "1", "1", "2", "3", "3", "4", "1", 
                                                                  "5", "5", "6", "7", "7", "8", "9", "10", "11", "12", "13"), admn_date = structure(c(18262, 
                                                                                                                                                      18264, 18277, 18279, 18283, 18262, 18264, 18277, 18287, 18275, 
                                                                                                                                                      18301, 18283, 18366, 18375, 18338, 18319, 18364, 18303, 18328, 
                                                                                                                                                      18341), class = "Date"), discharge_date = structure(c(18275, 
                                                                                                                                                                                                            18276, 18288, 18280, 18286, 18275, 18278, 18288, 18291, 18283, 
                                                                                                                                                                                                            18309, 18297, 18375, 18381, 18347, 18328, 18367, 18309, 18329, 
                                                                                                                                                                                                            18344), class = "Date")), row.names = c(NA, -20L), class = "data.frame")
df %>% 
  inner_join(df, join_by(
    patient_id == patient_id,
    #hosp_id != hosp_id,
    within(admn_date, discharge_date, admn_date, discharge_date)
  )) %>% 
  filter(row_id.x != row_id.y,
         hosp_id.x != hosp_id.y)
