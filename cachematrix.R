## Pair of functions that cache the inverse of a matrix.


##The makeCacheMatrix function creates a speacial "matrix" object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
  
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
    
  }
  get <- function() x
  setInv <- function(inverse) s <<- inverse
  getInv <- function() s
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve function computes the inverse of the special "matrix" using solve function, returned by makeCacheMatrix above.

## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getInv()
  if(!is.nunll(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setInv(s)
  s
}
