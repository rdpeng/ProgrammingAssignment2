
## The two functions makeCacheMatrix and cacheSolve calculate the inverse of a 
## (non-singular) square matrix. They demonstrate the use of lexical scoping in R, 
## by caching in inverse, and saving on the need to recalculate repeatedly. 

## makeCacheMatrix creates an R object from a passed numerical matrix x as an argument. 
## The object contains two data objects - the matrix x, and its inverse inv, as well as 
## four functions that act as getters (get() and getinv()) and setters (set(y) and setinv(solve)) 
## of the x and inv

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y) {
        x <<- y
        inv <<- NULL
        }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve takes an object of type makeCacheMatrix, and retrieves the inverse). More details:
## If the inverse is calculated (i.e. getinv is not NULL), returns the inverse from cache 
## If the inverse is not yet calculated (i.e. getinv returns NULL), calculates
## inverse using solve and stores in cache using setinv

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
       
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
