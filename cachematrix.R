## Caching the inverse of a matrix

## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setinverse <- function(solve) {inv <<- solve}
  getinverse <- function() {inv}
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse
       )
}


## Computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above.

cacheSolve <- function(x, ...) {
       inv <- x$getinverse()
       if(!is.null(inv)){
         message("getting cache data")
         return(inv)
       }
       mtx <- x$get()
       inv <- solve(mtx, ...)
       x$setinverse(inv)
       inv
}
