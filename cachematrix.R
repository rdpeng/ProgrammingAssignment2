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