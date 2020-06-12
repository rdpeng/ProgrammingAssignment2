## The functions below describe how to create a 
## special "matrix" object and cache the its inverse

## The function below creates a matrix 

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(a){
            x <<- a
            inv <<- NULL
      }
      get <- function() x 
      setmatinverse <- function(inverse) inv <<- inverse
      getmatinverse <- function() {inv}
      list(set = set, get = get,
            setmatinverse = setmatinverse,
            getmatinverse = getmatinverse)
}

## The function below computes the inverse of the above special 
## matrix created i.e. makeCacheMatrix. This function also returns 
## the inverse of the matrix if the matrix is computed and unchanged.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getmatinverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      mat <- x$get()
      inv <- solve(mat, ...)
      x$setmatinverse(inv)
      inv
}