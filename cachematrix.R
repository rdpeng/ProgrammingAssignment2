## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {         ## argument definition with default type matrix
  inv <- NULL                                       ## inv to hold matrix inverse, initialized to NULL
  set <- function(y){                               ## function to assign new matrix value in the parent environment
    x <<- y
    inv <<- NULL                                    ## reset inv to NULL if it's a new matrix
  }
  get <- function() x                               ## this function returns the value of the matrix argument
  setinverse <- function(inverse) inv <<- inverse   ## assigns the value of inv in parent environment
  getinverse <- function() inv                      ## gets value of inv in calling environment                     
  list(set = set, get = get,                        ## assigning names to refer above functions with $ operator
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then this function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}