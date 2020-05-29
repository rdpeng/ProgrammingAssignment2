matriz<- matrix(sample.int(10, 9, replace = TRUE), nrow = 3, ncol = 3)

makeCacheMatrix <- function(m.matrix = matrix()) {
  i.matrix<- NULL
  set <- function(y) {
    m.matrix<<- y
    i.matrix<<- NULL
  }
  get <- function() m.matrix
  set.inverse <- function(inverse.matrix) i.matrix <<-inverse.matrix
  get.inverse <- function() i.matrix
  list(set = set, get = get, set.inverse = set.inverse, get.inverse = get.inverse)
}


cacheInverse<- function(m.matrix, ...) {
  i.matrix<- m.matrix$get.inverse()
  if(!is.null(i.matrix)) {
    message("getting cached data")
    return(i.matrix)
  }
  data <- m.matrix$get()
  i.matrix<- solve(data, ...)
  m.matrix$set.inverse(i.matrix)
  i.matrix
}


matriz1<- makeCacheMatrix(matriz)


cacheInverse(matriz1)
