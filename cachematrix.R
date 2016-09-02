## Function to create an inverse of a matrix
## Returns cached inverse if already exists

## Creates a list of gets/sets for a matrix and the inverse of the matrix
 makeCacheMatrix <- function(x = matrix) {
   m <- NULL
   set <- function(y) {
     x <<- y
     m <<- NULL
   }
   get <- function() x
   setinverse <- function(solve) m <<- solve
   getinverse <- function() m
   list(set=set, get=get, 
        setinverse=setinverse,
        getinverse=getinverse)
 }
 
 # Uses solve function to return the inverse of the matrix, using cached version if one exists,
 # solving if one does not
 cacheSolve <- function(x, ...) {
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
