# Functions that compute the inverse of a matrix using cache.

# Caching function for a matrix argument.  
#Returns a vector of get; set for the matrix itself, 
#as well as 
# setinverse and getinverse for the matrix inverse calculation.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# Either calculates the inverse of a square matrix, 
# or retrieves a previously calculated inverse from cache, 
#given a list argument from the function makeCacheMatrix().

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("fetching cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data) %*% data
  x$setinverse(m)
  m
}