

origin <- partnership_summary$batsman_out_name
destination <- partnership_summary$batsman_in_name
data <- data.frame(origin, destination)

# Transform input data in a adjacency matrix
adjacencyData <- with(data, table(origin, destination))

# Charge the circlize library
library(circlize)
library(igraph)
library(tidygraph)
library(chorddiag)


# Make the circular plot
ggplotly(chordDiagram(adjacencyData, transparency = 0.5))

partnership_chord_data <- partnership_summary |> 
  mutate(first_bat = case_when(batsman_out_name < batsman_in_name ~ batsman_out_name,
                               TRUE ~ batsman_in_name),
         second_bat = case_when(batsman_out_name < batsman_in_name ~ batsman_in_name,
                                TRUE ~ batsman_out_name)) |> 
  select(first_bat, second_bat, partnership_runs) |> 
  filter(partnership_runs > 0) |> 
  arrange(first_bat, second_bat)

#duplicate partnership chord data
partnership_chord_data_dupli <- partnership_chord_data |> 
  select(second_bat = first_bat, first_bat = second_bat, partnership_runs)

partnership_chord_data_all <- partnership_chord_data |> 
  bind_rows(partnership_chord_data_dupli)

partnership_chord_matrix<-as.matrix(as_adjacency_matrix(as_tbl_graph(partnership_chord_data_all),attr = "partnership_runs"))

mig_data_filter[mig_data_filter == 0] <- NA

chordDiagramFromDataFrame(mig_data_filter)

chord<-chorddiag(data = partnership_chord_matrix,
                 groupnamePadding = 30,
                 groupPadding = 3,
                 groupColors = c("#ffffe5","#fff7bc","#fee391","#fec44f","#fe9929","#ec7014","#cc4c02","#8c2d04"),
                 groupnameFontsize = 13 ,
                 showTicks = FALSE,
                 margin=150,
                 tooltipGroupConnector = "    &#x25B6;    ",
                 chordedgeColor = "#B3B6B7"
)

chord<-chorddiag(data = mig_data_filter)
chord




titanic_tbl <- tibble::as_tibble(Titanic)
titanic_tbl <- titanic_tbl %>%
  mutate(across(where(is.character), as.factor))
by_class_survival <- titanic_tbl %>%
  group_by(Class, Survived) %>%
  summarise(Count = sum(n)) %>% 
  ungroup()
titanic.mat <- matrix(by_class_survival$Count, nrow = 4, ncol = 2, byrow = TRUE)
dimnames(titanic.mat ) <- list(Class = levels(titanic_tbl$Class),
                               Survival = levels(titanic_tbl$Survived))

groupColors <- c("#2171b5", "#6baed6", "#bdd7e7", "#bababa", "#d7191c", "#1a9641")
chorddiag(titanic.mat, type = "bipartite", 
          groupColors = groupColors,
          tickInterval = 50)






# Libraries
library(tidyverse)
library(viridis)
library(patchwork)
library(hrbrthemes)
library(igraph)
library(ggraph)
library(colormap)

# A really simple edge list
links=data.frame(
  source=c("A", "A", "A", "A", "B"),
  target=c("B", "C", "D", "F","E")
)

# Transform to a igraph object
mygraph <- graph_from_data_frame(mig_data_filter)

# Make the usual network diagram
p1 <-  ggraph(mygraph) + 
  geom_edge_link(edge_colour="black", edge_alpha=0.3, edge_width=0.2) +
  geom_node_point( color="#69b3a2", size=5) +
  geom_node_text( aes(label=name), repel = TRUE, size=8, color="#69b3a2") +
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(rep(2,4), "cm")
  ) 

# Make a cord diagram
p2 <-  ggraph(mygraph, layout="linear", circular = TRUE) + 
  geom_edge_arc(edge_colour="black", edge_alpha=0.3, edge_width=0.2) +
  geom_node_point( color="#69b3a2", size=5) +
  geom_node_text( aes(label=name), repel = FALSE, size=2, color="#69b3a2") +
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(rep(2,4), "cm")
  )

p2

# plot arc diagram
arcplot(mygraph, ordering=new_ord, labels=vlabels, cex.labels=0.8,
        show.nodes=TRUE, col.nodes=vborders, bg.nodes=vfill,
        cex.nodes = log(degrees)+0.5, pch.nodes=21,
        lwd.nodes = 2, line=-0.5,
        col.arcs = hsv(0, 0, 0.2, 0.25), lwd.arcs = 1.5 * values)
