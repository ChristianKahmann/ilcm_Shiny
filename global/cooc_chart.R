
scalecolors <- function(nodes, palette) {
  n <- max(unique(nodes$group))
  cols <- rev(RColorBrewer::brewer.pal(n, palette))
  cols <- paste0("'", paste(cols, collapse = "', '"), "'")
  networkD3::JS(paste0('d3.scaleOrdinal().domain([0,', n, ']).range([', cols, '])'))
}

coocsChart <- function(nodes,links,charge){
  
  
  g <- igraph::graph_from_data_frame(links, directed = FALSE, vertices = NULL)
  names(igraph::edge_attr(g))[which(names(igraph::edge_attr(g)) == "value")] <- "weight"
  b <- igraph::betweenness(g, v = igraph::V(g), directed = F, #weights = NULL,
                   nobigint = TRUE, normalized = FALSE)
  
  MyClickScript <- 'Shiny.onInputChange("cooc_word", d.name);'
  #alert("You clicked " + d.name + " which is in row " +
  #        (d.index + 1) +  " of your original R data frame");'
  
  #Wether if arrows are in
  arrows <- F
  
  #nodes[1,"w_id"] <- 0
  nodes[,"name"] <- factor(nodes[,"name"])
  nodes[,"betweenness"] <- sqrt(b)
  
  F2 <- colorRampPalette(c("#222222", "#C1FD33"), bias = nrow(links), space = "rgb", interpolate = "linear")
  colCodes <- F2(length(unique(links$value)))
  edges_col <- sapply(links$value, function(x) colCodes[which(sort(unique(links$value)) == x)])
  
  r1 <- networkD3::forceNetwork(Links = links, Nodes = nodes,
                     Source = "source", Target = "target",
                     Value = "value", NodeID = "name",
                     Nodesize = 'betweenness',
                     charge = charge,#Variabler Paramerter über Slider
                     colourScale = scalecolors(nodes, 'RdBu'),
                     linkColour = edges_col,
                     Group = "group", opacity = 1,
                     fontFamily = "arial",
                     linkWidth = JS("function(d) { return Math.log(d.value)-2; }"),
                     linkDistance = networkD3::JS("function(d) { return 10*(d.value); }"),
                     #linkColour = "#333333",
                     fontSize = 15, opacityNoHover = 0.9,
                     arrows = arrows,
                     radiusCalculation = " Math.sqrt(d.nodesize)+6",
                     zoom = T,
                     bounded = T,
                     clickAction = MyClickScript
                     )
   return(r1)
  
  
}