## Taking two function to first set callable global values of a matrix 
## to be cached and then outputting the inverse of that matrix

## Function to set of global environment values for the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL                   
  set <- function(y) {        
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

## Call global parameters to generate the inverse of the matrix 
## from the function makeCacheMatrix

cacheSolve <- function(x, ...) {
   i <- x$getinverse()
   if (!is.null(i)) {
     message ("getting cached data")
     return(i)
   }
   data <- x$get()
   i <- solve(data, ...)
   x$setinverse(i)
   i
}
