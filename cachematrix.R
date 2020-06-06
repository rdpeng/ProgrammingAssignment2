# This pair of functions cache the inverse of a matrix

# makeCacheMatrix: This function creates a special "matrix" object that can 
# cache its inverse

makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setsolve <- function(solve) {
    if(det(x) == 0)
      stop("Determinant of 'x' is 0, cannot be inverted")
    else if(dim(x)[1] != dim(x)[2])
      stop("'x' must be square, cannot be inverted")
    else
      s <<- solve
  }
  getsolve <- function() m
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

# cacheSolve: This function computes the inverse of a special "matrix" returned 
# by makeCacheMatrix above. If the inverse has already been calculated (and the
# matrix has not changed), then the cache solve should retrieve the inverse from
# the cache

cacheSolve <- function(x, ...){
  m <- x$getsolve()
  
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
