## these functions are used to cache the inverse of a matrix, if a matrix has been inversed, get it from the cache,
## otherwise caculate and cache it.

## This function create a special matrix and make it to cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
        inv <<- NULL
        set<- function (y){
               x<<- y
               inv<<- NULL
        }
        get<- function() x
        setinverse<- function(inverse) inv<<- inverse
        getinverse<- function() inv
        list( set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This function caculate the inverse of matrix returned by makeCacheMatrix. If the inverse has been caculated, 
## get it from cache. If not, caculate and cache it

cacheSolve <- function(x, ...) {
        inv<- x$getinverse()
      if(!is.null(inv)) {
              message("getting cached data")
              return(inv)
      }
      matrix<- x$get()
      inv<- solve(matrix)
      x$setinverse(inv)
      inv
} 
