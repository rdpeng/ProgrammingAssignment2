## Put comments here that give an overall description of what your
## functions do

## The first function, makeCacheMatrix creates a special “matrix”, which is 
##really a list containing a function to: set the elements of the matrix,
##get the elements of the matrix, set the elements of the matrix inverse, 
##get the elements of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function return a inverse of special “matirx” created with
##the above function.However, it first checks to see if the inverse has already
##been calculated.If so, it gets the inverse from the cache and skips the 
##computation.Otherwise, it calculates the inverse of the matrix and sets the 
##value of the inverse in the cache via the set_inverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}



