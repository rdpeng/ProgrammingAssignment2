# R Programming Assignment 2: Lexical Scoping--caching the inverse of a matrix


makeCacheMatrix <- function(l = matrix()) {
  h <- NULL
  set <- function(p) {
    l <<- p
    h <<- NULL
  }
  get <- function() l
  setinverse <- function(inverse) h <<- inverse
  getinverse <- function() h
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


# The cacheSolve function calculates the inverse of the special "matrix"
# the special "matrix" which created with the makeCacheMatrix function.
# However, it first checks to see if the inverse has already been calculated.
# If so, it gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the matrix and sets the value of the inverse
# in the cache via the setinverse function.


cacheSolve <- function(l, ...) {
  ## Return a matrix that is the inverse of 'l'
  h <- l$getinverse()
  if(!is.null(h)) {
    message("getting cached data")
    return(h)
  }
  data <- l$get()
  h <- solve(data, ...)
  l$setinverse(h)
  h
}
