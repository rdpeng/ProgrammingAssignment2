## The first function,’makeCacheMatrix’creates a special “vector” that contain a function to:

## set the matrix in cache
##get the matrix from cache
##set the inverse matrix into cache
##get the inverse matrix from cache

> makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
     x <<- y
     m <<- NULL
   }
   get <- function() x
   setsolve <- function(solve) m <<- solve
   getsolve <- function () m
   list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
 }


## Cachesolve checks if the matrix is solved,if it is solved it returns the matrix
##otherwise it inverts the matrix and sets it as ‘setinverse’

cacheSolve <- function(x, ...) {
   m <- x$getsolve()
   if(!is.null(m)) {
     message("getting cached data")
     return(m)
   }
   data <- x$get()
   m <- solve(data, ...)
   x$setsolve(m)
   m
 } 

