makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setinverse <- function(inverse) {inv <<- inverse}
  getinverse <- function() {nv}
  list(set = set, get = get, setInverse = setinverse, getiverse = getinverse)
}

cachesolve <- function(x, ...){
  inv <- x$getInverse()
  if (!is.null(inv)){
         message("getting cached data")
         return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}  
