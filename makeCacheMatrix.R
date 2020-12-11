makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
         x <<- y
         inv <<- NULL
  }
  get <- function() (x)
  setinverse <- function(inverse) {inv <<- inverse}
  getinverse <- function() (inv)
  list(set = set,get = get, setInverse = setInverse, getInverse = getInverse )
}

CacheSolve <- function(x,...) {
     inv <- x$getinverse()
     if(!is.null(inv)){
         message("getting cached data")
        return(inv)
     }
     mat <- x$get()
     inv <- solve(data,...)
     x$setinverse(inv)
     inv
}