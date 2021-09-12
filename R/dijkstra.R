# The dijkstra function return the shortest path to every
# other node from the starting node as a vector.
#
# You can learn more about the algorithm at:
#
#   https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
#
# Input in the function (graph, init_node) must be:
# - Graph: dataframe & contain columns ("v1", "v2", "w").
# - init_node: be a numeric scalar.

#wiki_graph <-
#  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
#             v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#             w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))

dijkstra <-function(graph, init_node){

  # Make sure input is valid.
  if(
    is.data.frame(graph) &&
    all(c("v1", "v2", "w") %in% colnames(graph))&&
    (is.atomic(init_node) && length(init_node) == 1L)

  ){
    # Define que & visited nodes
    que <- unique(graph[['v1']]) # vector with the nodes
    visited <- c()

    # Distance vector
    distance_vector <- rep(Inf, length=length(que))
    distance_vector[init_node] <- 0

    while(length(que) > 0){

      shortest_distance = Inf

      # Find smallest corresponding value of elements still in que
      for(y in seq_along(que)){

        # Index of element
        ind <- que[y]

        if(distance_vector[ind] < shortest_distance){
          shortest_distance <- distance_vector[ind]
          shortest_index <- ind
        }

      }
      # Update que and visited nodes
      visited <- c(visited, shortest_index)
      que <- que[que!= shortest_index]

      # Check neighbours
      neighbour_df <- graph[graph$v1==shortest_index,]

      # For each neighbour row of the current node
      for (row in 1:nrow(neighbour_df)){
        # Check if neighbour have been removed from que
        if(!neighbour_df[row,'v2'] %in% que){
          next
        }
        else{ # If not, calculate path
          alternative_path <- distance_vector[shortest_index] + neighbour_df[row,'w']
          # If alternative path is shorter, update
          if(alternative_path < distance_vector[neighbour_df[row, 'v2']]){
            distance_vector[neighbour_df[row, 'v2']] <- alternative_path
          }
        }
      }
    }

  return(distance_vector)

  }else{
    stop("Invalid input.")
  }

}
#dijkstra(graph = wiki_graph, 1)


