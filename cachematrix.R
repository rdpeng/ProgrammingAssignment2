## The first function makeCacheMatrix creates a special "matrix" and caches its inverse.
## The second function checks whether the inverse of the "matrix" has been calculated.
## If the inverse already exists, it retrieves the inverse from the cache and does not calculate again.
## If the inverse has not been created, it calculates the inverse of the matrix and sets it in the cache.

## makeCacheMatrix creates a special "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      inv<-NULL
      set<-function(y){
            x<<-y
            inv<<-NULL
      }
      get <- function()x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set=set, get=get,
           setinverse=setinverse,
           getinverse=getinverse)
      
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calcuated, then the cacheSolve retrieves the inverse from the cache.


cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}
