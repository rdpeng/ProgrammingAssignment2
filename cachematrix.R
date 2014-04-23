## Cache the inverse of a matrix to avoid computing it 
## repeatly. 

## makeCacheMatrix creates a special "matrix", which is
## a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)  
}


## cacheSolve calculates the inverse of the matrix created
## with the makeCacheMatrix function. It first checks to 
## see if the inverse has already been caculated. If so, it
## gets the inverse from cache and skips the computation.
## Otherwise it caculates the inverse of the matrix and sets
## the value of the inverse in the cache via the setInverse
## method.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m  
}
