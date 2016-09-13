# 1.1.2
dijkstra <- function(graph, init_node){
  # initiate unvisited vertex set
  vertexQ = unique(graph$v1)
  
  # initiate result table
  rd = data.frame(matrix(nrow=length(unique(graph[,1])), ncol=2))
  colnames(rd) = c("dist", "prev")
  rd$dist = Inf
 
  # distance from source to source
  rd[init_node, ]$dist = 0
  
  while(length(vertexQ) > 0){
    for(u in vertexQ){
      if(rd[u, ]$dist != min(rd[vertexQ, ]$dist)){
        next
      }
      
      # remove u from unvisited set
      vertexQ = vertexQ[!vertexQ %in% u]
      
      # get neignbors of u that are still in Q
      ng = graph[graph$v1 == u, ]
      nv = ng$v2
      nv = nv[nv %in% vertexQ]
      
      # update path of neighbours
      for(v in nv){
        alt = rd[u, ]$dist + ng[ng$v2 == v, ]$w
        if(alt < rd[v, ]$dist){
          rd[v, ]$dist = alt
          rd[v, ]$prev = u
        }
      }
    }
    
  }
  rd$dist

}

wiki_graph <-
  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
             v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
             w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))

dijkstra(wiki_graph, 1)
dijkstra(wiki_graph, 3)


