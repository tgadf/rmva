library(tidyr)
library(dplyr)
library(tibble)
library(igraph)
library(visNetwork)
library(RColorBrewer)

# Options
size_adjustment <- 20
sf <- 6

# Read data
#df <- read.csv("~/network/dsteam.csv", sep = "\t", stringsAsFactors = FALSE)
df <- read.csv("~/Copy of SLT_Atlanta_Individual Top 5_SRv5.csv", sep = ",", stringsAsFactors = FALSE)
#df <- read.csv("~/tmp.csv", sep = ",", stringsAsFactors = FALSE)

df_desc <- read.csv("~/network/aa-strengthmap-clee/All_34_Strengths_Brief_Description.txt", sep = "\t", stringsAsFactors = FALSE)
strength_areas <- unique(df_desc$Leadership.Domains)
domains <- strength_areas
df_descs <- list()
for ( domain in domains ) {
  df_descs[[domain]] <- df_desc[df_desc$Leadership.Domains == domain,]
}

colfunc <- colorRampPalette(c("orange", "red"))
cols <- colfunc(4)
cols <- c(tail(brewer.pal(4,"Paired"), n=2), tail(brewer.pal(8,"Paired"), n=2))
#cols <- c(tail(brewer.pal(4,"Paired"), n=2), tail(brewer.pal(10,"Paired"), n=2))
cols <- c(tail(brewer.pal(4,"Paired"), n=2), tail(brewer.pal(12,"Paired"), n=2))
cols <- c(cols[1], cols[3], cols[4], cols[2])
cols <- brewer.pal(8,"Dark2")
cols <- c(cols[1], cols[2], cols[4], cols[6])
cols <- brewer.pal(4,"Pastel2")
usercol <- 'black'
cols <- c("#375E97", "#FB6442", "#FFBB00", "#3F681C") #blue,red,yellow,green
#cols <- c("#363237", "#2D4262", "#73605B", "#D09683") #purple, indigo,taupe,blush
#cols <- c("#2D2300", "#6E6702", "#C05805", "#DB9501") #bark, green, bronze, goldrod
#cols <- c("#", "#", "#", "#")
#cols <- c("#", "#", "#", "#")
#cols <- c("#7F152E", "#D61800", "#EDAE01", "#E94F08") # no good

#cols <- c(tail(brewer.pal(8,"Paired"), n=4))
#cols <- brewer.pal(4,"Accent")
#cols <- brewer.pal(4,"Dark2")
#cols <- brewer.pal(4,"Set2")


# Define edges
edges_name <- df %>% 
  gather(key = Strength, value = edge, -Name) %>% 
  filter(edge == 1) %>%
  select(from = Name, to = Strength) 

edges_area <- df_desc %>%
  select(from = Leadership.Domains, to = Strengths)

edges <- edges_name %>%
  bind_rows(edges_area)


# Define nodes
nodes_name <- edges_name %>% 
  distinct(from) %>% 
  dplyr::rename(id = from) %>% 
  dplyr::mutate(group = "Name", shape = "icon", icon.code = "f007", color = "lightblue")


size_strength <- edges_name %>% 
  group_by(to) %>% 
  dplyr::count() %>%
  dplyr::rename(id = to, size = n) %>%
  dplyr::mutate(size = sf*size + size_adjustment)


nodes_strength <- edges_name %>% 
  distinct(to) %>% 
  dplyr::rename(id = to) %>%
  dplyr::mutate(group = "Strength") %>%
  inner_join(size_strength) %>%
  inner_join(
    df_desc %>%
      select(id = Strengths)
  )
#, title = Brief.Descriptions)

nodes_strengths <- list()
for ( domain in domains ) {
  nodes_strengths[[domain]] <- edges_name %>% 
    distinct(to) %>% 
    dplyr::rename(id = to) %>%
    dplyr::mutate(group = domain) %>%
    inner_join(size_strength) %>%
    inner_join(
      df_descs[[domain]] %>%
        select(id = Strengths)
    )
}


#, title = Brief.Descriptions)
size_area <- edges_area %>%
  dplyr::rename(id = to) %>%
  inner_join(size_strength) %>%
  group_by(from) %>%
  dplyr::summarise(size = sum(sf*size - size_adjustment) + size_adjustment) %>%
  dplyr::rename(id = from)



nodes_area <- edges_area %>% 
  distinct(from) %>% 
  dplyr::rename(id = from) %>% 
  mutate(group = "Strength Area") %>%
  inner_join(size_area)


nodes_areas <- list()
for ( domain in domains ) {
  nodes_areas[[domain]] <- nodes_area[nodes_area$id == domain,]
  nodes_areas[[domain]][,"group"] <- paste(domain,"Domain") #paste(nodes_areas[[domain]][,"group"],domain)
  nodes_areas[[domain]][,"size"]  <- nodes_areas[[domain]][,"size"]/11
}
##print(nodes_areas)

nodes <- nodes_name %>%
  bind_rows(nodes_areas[[domains[1]]]) %>%
  bind_rows(nodes_areas[[domains[2]]]) %>%
  bind_rows(nodes_areas[[domains[3]]]) %>%
  bind_rows(nodes_areas[[domains[4]]]) %>%
  bind_rows(nodes_strengths[[domains[1]]]) %>%
  bind_rows(nodes_strengths[[domains[2]]]) %>%
  bind_rows(nodes_strengths[[domains[3]]]) %>%
  bind_rows(nodes_strengths[[domains[4]]]) %>%
  dplyr::mutate(label = id)


writeLines("Creating network")
print(nodes)

#visGroups(groupname = domains[1], shape = "square", color = cols[1]) %>%
#  visGroups(groupname = domains[2], shape = "square", color = cols[2]) %>%
#  visGroups(groupname = domains[3], shape = "square", color = cols[3]) %>%
#  visGroups(groupname = domains[4], shape = "square", color = cols[4]) %>%
#bind_rows(nodes_areas[[1]]) %>%
#  bind_rows(nodes_areas[[2]]) %>%
#  bind_rows(nodes_areas[[3]]) %>%
#  bind_rows(nodes_areas[[4]]) %>%

lnodes <- data.frame(label = paste(domains,"Domain"),
                     shape = c("dot"), 
                     color = cols)

#color = list(color = "lightblue"
# Visualize graph
#visGroups(groupname = "Name", size = 50) %>%
shape <- "square"
domainshape <- "dot"
visNetwork(nodes = nodes, edges = edges, 
           main = "SLT Strength Network Graph",
           height = "600px", width = "1080px") %>%
  visEdges(width=4, color = list(color = "lightblue", highlight = "navy")) %>%
  visNodes(font = list(size = 95)) %>%
  visGroups(groupname = paste(domains[1],"Domain"), shape = domainshape, color = cols[1]) %>% #, font = list(size = 28)) %>%
  visGroups(groupname = paste(domains[2],"Domain"), shape = domainshape, color = cols[2]) %>% #, font = list(size = 28)) %>%
  visGroups(groupname = paste(domains[3],"Domain"), shape = domainshape, color = cols[3]) %>% #, font = list(size = 28)) %>%
  visGroups(groupname = paste(domains[4],"Domain"), shape = domainshape, color = cols[4]) %>% #, font = list(size = 28)) %>%
  visGroups(groupname = domains[1], shape = shape, color = cols[1]) %>% #, font = list(size = 28)) %>%
  visGroups(groupname = domains[2], shape = shape, color = cols[2]) %>% #, font = list(size = 28)) %>%
  visGroups(groupname = domains[3], shape = shape, color = cols[3]) %>% #, font = list(size = 28)) %>%
  visGroups(groupname = domains[4], shape = shape, color = cols[4]) %>% #, font = list(size = 28)) %>%
  visGroups(groupname = "Name", shape = "icon", color=usercol, icon = list(color = usercol, code = "f007", size = 100)) %>%
  addFontAwesome() %>%
  visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T), nodesIdSelection = T) %>%
  visLegend(addNodes = lnodes, useGroups = FALSE) %>%
  visLayout(randomSeed =21, improvedLayout = TRUE) %>%
  visInteraction(navigationButtons = TRUE) %>%
  visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -500))
  #visPhysics(barnesHut = list(gravitationalConstant = -52500))
  #list(label = "User", shape = "icon", 
#     icon = list(code = "f007", size = 20, color = "dodgerblue"))), 
# %>%  #, addNodes = list(useGroups = TRUE)) %>%
#visHierarchicalLayout(direction = "LR", levelSeparation = 500) %>%
    