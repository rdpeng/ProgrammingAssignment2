## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
     x <- NULL
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


## Write a short comment describing this function
## <input>This function take a matrix as argument and addition 
##        argument for solve function</input>
## <summary>This function get cached invers if exist any, 
##          OR create invers and store it to cache</summary>
#=====================================================================================
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
