makeCacheMatrix <- function(x = matrix()) {
  ## This function creates a special "matrix" object that can cache its inverse
  
  makeCacheMatrix <- function(x = matrix()) { ## define the argument with default mode of "matrix"
    Inv <- NULL                             ## initialize inv as NULL; will hold value of matrix inverse 
    set <- function(y) {                    ## define the set function to assign new 
      x <<- y                             ## value of matrix in parent environment
      Inv <<- NULL                        ## if there is a new matrix, reset inv to NULL
    }
    get <- function() x                     
    
    setinverse <- function(inverse) Inv <<- inverse  ## assigns value of inv in parent environment
    getinverse <- function() Inv                     ## gets the value of inv where called
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
  }
  
}

cacheSolve <- function(x, ...) {
  Inv <- x$getinverse()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  data <- x$get()
  Inv <- solve(data, ...)
  x$setinverse(Inv)
  Inv
}
