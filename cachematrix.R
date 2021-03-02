## makeCacheMatrix is a function with several functions inside.  The first function establishes 
## additional functions cacheSolve to call from. 
## i is the global variable that will hold the matrix.  
 

## This function pulls in a matrix and provides functions for cacheSolve to store into memory

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve checks for a cached matrix, if one doesn't exist it creates the inverse and stores it.

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


## I copied the 2 functions provided in the exercise and converted m to i.  
## Next, I converted means to inverse.  Finally, I added the solve function.




