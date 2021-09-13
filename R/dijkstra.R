#' @title dijkstra
#' @description See:
#' # \url{https://en.wikipedia.org/wiki/Dijkstra\%27s_algorithm}{Dijkstra}
#' @param graph A datafram containing 3 columns ("v1", "v2", "w").
#' @param init_node A numeric scalar.
#'
#' @return The dijkstra of \code{graph} and \code{init_node}.
#'
#' @export

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


