## below are two functions, one calculate the inverse of the Matrix and cached it, and
## the other solve matrix for its inverse, if it's cached it will call it directly, if
## not cached will calculate it.

## makeCacheMatrix will take Matrix as an argument and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverseofx <-NULL
   setmatrix <-function(y){
     x <<- y
     inverseofx <<- NULL
   }
   getmatrix <- function() x
   setinverse <- function(inverse) inverseofx <<- inverse
   getinverse <- function() inverseofx
   list (setmatrix=setmatrix, getmatrix=getmatrix,setinverse=setinverse,getinverse=getinverse)
   
}


## cacheSolve will either call the inverse of the Matrix from the cache or calculate and
## cached it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   inverseofx <- x$getinverse()
  if (!is.null(inverseofx)) {
    message("getting cached data")
    return(inverseofx)
  }
  matrix <- x$getmatrix()
  inverseofx <- solve(matrix)
  x$setinverse(inverseofx)
  inverseofx
}

