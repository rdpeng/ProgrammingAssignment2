## Functions that cache the inverse of a matrix

## Creates a matrix object that caches its inverse
makeCacheMatrix <- function(a = matrix()) {
  inv <- NULL
  set <- function(b) {
    a <<- b
    inv <<- NULL
  }
  get <- function() a
  setinversen <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##Computes the inverse of the matrix above
cacheSolve <- function(a, ...) {
  ## Return a matrix that is the inverse of 'a'
  inv <- a$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")  #getting the data from directory  cache
    return(inv)
  }
  data <- a$get()
  inv <- solve(data, ...) # assigning inv to be solved
  a$setinverse(inv)
  inv
}
