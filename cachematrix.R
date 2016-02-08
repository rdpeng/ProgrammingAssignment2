## makeCacheMatrix create special matrix which cache the inverse
## cacheSolve return inverse of special matrix

## This function create a specia Matrix for which inverse can be cache

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve returns inverse of special matix created using makeCacheMatrix

cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  
  #in inverse is already calculated return cached result
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  
  #calculate inverse using solve function
  
  inv <- solve(data, ...)
  
  # Cache the inverse using set function
  x$setinverse(inv)
  inv
}
