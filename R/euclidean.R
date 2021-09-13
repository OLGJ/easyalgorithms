#' @title euclidean
#' @description \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#'
#' @param x A numeric input.
#' @param y A numeric input.
#'
#' @return The Euclidean of \code{x} and \code{y}.
#'
#' @export


euclidean <- function(x, y) {

  # Input must be numeric
  if (is.numeric(x) && is.numeric(y)){
    while(y != 0){
      temp <- y
      y <- x %% y
      x <- temp
    }
  }else{
    stop("Invalid input")
  }
  return(x)
}
