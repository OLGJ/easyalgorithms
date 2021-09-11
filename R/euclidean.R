# The function euclidean uses the euclidean division algorithm.
#
# You can learn more about the algorithm at:
#
#   https://en.wikipedia.org/wiki/Euclidean_algorithm
#
# Input in the function (x, y) must be numeric.

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
