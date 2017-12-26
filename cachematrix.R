## Creates a Special Vector that allows us to cache the inverse
## of a matrix using two functions as shown below
## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function (m) {
  inv <- NULL 
set <- function(x) { 
m <<- x
inv <<- NULL
  }
  
  get <- function() m ## prints the original matrix
  setinverse <- function(y) inv <<- y ##stores the inverse
  getinverse <- function() inv ##prints the inverse from the cache
  list(set = set,get = get, setinverse = setinverse, getinverse = getinverse)
 }

## Computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function (l, ...) {
inv <- l$getinverse()
## Checking whether matrix is already cached!
if(!is.null(inv)) {
message("Getting cashed data!")
return(inv)
}
## if there is no stored inverse..
data <- l$get()
inv <- solve(data, ...) ##function to get the inverse of a matrix
l$setinverse(inv) ##storing the inverse
inv # Returning the inverse
}

