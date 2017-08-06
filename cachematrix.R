##Function that caches the inverse of a matrix so it's not repeated calculated 
##Pass the matrix, store the matrix, get its inverse, and cache the inverse for future use
## Create a list that contains where the inverse has been calculated and if so the inverse so it's not recalculated
## Assumes matrix passed to function is square and therefore is invertible


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }  
  


## Get the inverse of a matrix using the Solve function
## If already calculated, then return the inverse from cache; otherwise calculate the inverse and cache it

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
