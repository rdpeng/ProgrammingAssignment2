## First, we create a matrix and than a list of the matrix and its inverse.
## The second function returns an inverse of a matrix, which is whether calculated 
## anew or loaded from the cache if it's been calculated before.

## Creates a list to set and get the value of a 
## matrix and set and get the value of its inverse

makeCacheMatrix <- function(M = matrix()) {
  m <- NULL
  set <- function(N) {
    M <<- N
    m <<- NULL
  }
  get <- function() M
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Calculates the inverse of the matrix from the list created above
## checks first, whether the value is already stored in the cache

cacheSolve <- function(M, ...) {
  m <- M$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- M$get()
  m <- solve(data, ...)
  M$setinv(m)
  m
}
