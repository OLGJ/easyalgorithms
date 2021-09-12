#' @title euclidean
#' @description \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#'
#' @param x A numeric input.
#' @param y A numeric input.
#'
#' @return The Euclidean of \code{x} and \code{y}.
#' @examples
#' euclidean (123612, 13892347912)
#' euclidean(100, 1000)

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
