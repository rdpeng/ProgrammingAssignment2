## creates a speal matrix to cache its inverse
  
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set<- function(y){
    x <<- y
    inv <<- NULL
  }
  get<- function() x
  setinverse<- function(inverse) inv <<- inverse
  getinverse<- function( ) inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## compute the inversion of the matrix 'x'

cacheSolve <- function(x, ...) {
       
  inv <- x$getinverse()
  if(!is.null(inv)) {      #checks to see if inverse already exists
    message("getting cached data") 
  return(inv)
  }
  data <- x$get()        #if inverse does not exist solves inverse of matrix
  inv <- solve(data, ...)
  x$setinverse(inv)
  return(inv)
}

