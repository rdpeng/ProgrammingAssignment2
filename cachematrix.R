## By creating the two functions below,you could combine them and save time finding the inverse of
## a large invertible matrix by caching inverses 

## This function is very similar to the example, it simply sets/gets the value of the matrix and 
## sets/gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes the cached inverse if it already exists and returns it as i; 
##if it does not exist it calculates the inverse then sets it as i

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("cached data")
    return(i)
  }
  data <- x$get()
  i <- inverse(data, ...)
  x$setinverse(i)
  i
}
