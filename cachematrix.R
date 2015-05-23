## The first function contains closures (functions written by another function). Closures are so called 
## because they enclose the environment of the parent function, and can access all variables and
## parameters in that function. This is useful because it allows to have two levels of parameters.
## The parent controls how the function works, the child level does the work.
## The second function actually calls the first function with the relevant matrix value.
## If the inverse value of this matrix has already been calculated, the cache value gets 
## returned (no extra calculation). Otherwise, the inverse gets calculated by the solve()-function,
## and the result gets returned.

## General procedure of the first function has been explained above. The <<- assignment operator
## makes it possible to modify variables in parent level.

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## As explained above the second function first checks if the inverse matrix for the relevant
## matrix parameter has already been calculated previously. If so, the cache value gets returned.
## Otherwise, a new calculation gets triggered returning the fresh computed result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
