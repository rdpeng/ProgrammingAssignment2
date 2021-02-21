## Both functions together can be used to cache the inverse of a matrix 
## and, if the inverse is already in the cache, retrieve it from there and return it.


## The function makeCacheMatrix() returns a list, containing functions.
## These functions get/set a matrix object in the environment of makeCacheMatrix
## and they set/get inverse of that matrix object.


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
       getinverse=getinverse,
       setinverse=setinverse)
}

## The function cacheSolve takes arguments of the type makeCacheMatrix.
## It gets the inverse via the getinverse() function which takes the inverse 
## from the environment of makeCachematrix.
## If the inverse has already been calculated it returns the cached inverse.
## IF it has not been calculated yet it calculates the inverse and sets it inside
## the makeCacheMatrix() environment

cacheSolve <- function(x, ...) { 
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
      
}

