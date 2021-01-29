get_Layout <- function(nodes = NULL, edges = NULL, year = NULL, layout = NULL, byClass = FALSE, simplified = FALSE)
{
  nodes_x <- nodes[which(nodes$Release <= year),]; nodes_x$Index <- 1:nrow(nodes_x);
  edges_x <- edges[which(edges$Year == year),]; edges_x$from <- as.numeric(unlist(left_join(edges_x, nodes_x[, c(1, 4)], by = c("from" = "Name"))[, 5])); edges_x$to <- as.numeric(unlist(left_join(edges_x, nodes_x[, c(1, 4)], by = c("to" = "Name"))[, 5]));
  
  worldsLoL_graph <- tbl_graph(nodes = nodes_x, edges = edges_x, directed = TRUE) %>%
    activate(nodes) %>%
    mutate(
           from_FLAG   = ifelse(Name %in% as.character(unlist(as.data.frame(edges[which(edges$Year == year), "from"]))), 1, 0),
           to_FLAG     = ifelse(Name %in% as.character(unlist(as.data.frame(edges[which(edges$Year == year), "to"]))),   1, 0),
           FLAG        = from_FLAG + to_FLAG,
           
           Degree      = replace(centrality_degree(), FLAG < 1, 0), 
           PageRank    = replace(centrality_pagerank(), FLAG < 1, 0), 
           Betweenness = replace(centrality_betweenness(), FLAG < 1, 0),
           Eigen       = replace(centrality_eigen(), FLAG < 1, 0)) %>% 
    select(-from_FLAG, -to_FLAG) %>%
    select(Name, Class, Release, Index, Degree, PageRank, Betweenness, Eigen, FLAG);
  
  if(simplified) {worldsLoL_graph <- get_Simplified(x = worldsLoL_graph);}
  
  layout_x <- create_layout(worldsLoL_graph, layout = layout); 
  
  if(byClass)
  {
    tmp <- layout_x[order(layout_x$x),]; tmp2 <- tmp[order(tmp$Class),][, -c(1:2)]; tmp[, c(3:14)] <- tmp2; rm(tmp2);
    layout_x$x <- left_join(layout_x, tmp[, c(1, 3)], by = c("Name"))[, ncol(layout_x) + 1];
    layout_x$y <- left_join(layout_x, tmp[, c(2, 3)], by = c("Name"))[, ncol(layout_x) + 1]; rm(tmp);
  }
  
  return(list("Graph"  = worldsLoL_graph,
              "Layout" = layout_x));
};

get_Simplified <- function(x = NULL)
{
 
  nodes <- x %>% activate(nodes) %>% data.frame();
  nodes <- nodes[which(nodes$FLAG != 0),] %>%
    group_by(Class) %>%
    mutate(av_Degree      = mean(Degree),
           av_PageRank    = mean(PageRank),
           av_Betweenness = mean(Betweenness),
           av_Eigen       = mean(Eigen)) %>%
    ungroup();
  nodes <- unique(nodes[, c("Class", "av_Degree", "av_PageRank", "av_Betweenness", "av_Eigen")]); 
  nodes$Index <- 1:6; nodes <- nodes[, c(1, 6, 2:5)]; colnames(nodes) <- c("Name", "Index", "Degree", "PageRank", "Betweenness", "Eigen");
  
  edges <- x %>% activate(nodes) %>% data.frame() %>% select(Class);
  edges <- unique(expand.grid(edges[, 1], edges[, 1])); edges <- edges[-which(edges[, 1] == edges[, 2]),]; colnames(edges) <- c("from", "to");
  edges$from <- as.numeric(unlist(left_join(edges, nodes[, c(1, 2)], by = c("from" = "Name"))[, 3])); edges$to <- as.numeric(unlist(left_join(edges, nodes[, c(1, 2)], by = c("to" = "Name"))[, 3]));
  
  return(tbl_graph(nodes = nodes, edges = edges, directed = TRUE));
};

get_Equal <- function(x = NULL, y = NULL)
{
  x$x <- left_join(x, y[, c(1, 3)], by = c("Name"))[, ncol(x) + 1];
  x$y <- left_join(x, y[, c(2, 3)], by = c("Name"))[, ncol(x) + 1];
  
  return(x);
};

get_Plots <- function(nodes = NULL, edges = NULL, layout = "fr", byClass = FALSE, centrality = NULL, simplified = FALSE)
{
  
  set.seed(1); Plots <- list(); list_Index <- 0; 
  graph_2020 <- get_Layout(nodes = nodes, edges = edges, year = 2020, layout = layout, byClass = byClass, simplified = simplified);

  for(i in 2011:2020)
  {
    if(i == 2020) 
    {
      list_Index = list_Index + 1;
      Plots[[list_Index]] <- ggraph(graph_2020[[2]]) + 
        geom_edge_link(if(!simplified) {aes(alpha = Weight)}) + 
        geom_node_point(if(byClass) {aes(color = Class, size = get(centrality))} else{if(!byClass & !simplified) {aes(size = get(centrality))} else{aes(color = Name, size = get(centrality))}}) +
        theme_graph(base_size = 14, base_family = "Times") + theme(legend.position = "none") +
        labs(title = paste0("", i)) + labs(col = "Class", size = "Eigenvector Centrality"); 
      
      return(Plots);
    }
    
    graph_x <- get_Layout(nodes = nodes, edges = edges, year = i, layout = "fr", simplified = simplified);
    graph_x[[2]] <- get_Equal(x = graph_x[[2]], y = graph_2020[[2]]);
    
    list_Index = list_Index + 1;
    Plots[[list_Index]] <- ggraph(graph_x[[2]]) + 
      geom_edge_link(if(!simplified) {aes(alpha = Weight)}) + 
      geom_node_point(if(byClass) {aes(color = Class, size = get(centrality))} else{if(!byClass & !simplified) {aes(size = get(centrality))} else{aes(color = Name, size = get(centrality))}}) +
      theme_graph(base_size = 14, base_family = "Times") + theme(legend.position = "none") + 
      labs(title = paste0("", i)) + labs(col = "Class", size = "Eigenvector Centrality"); 
  }
  
}