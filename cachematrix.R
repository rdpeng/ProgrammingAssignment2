## This function is used to first cache the inverse ofthe matrix and later 
## make use of this result instead of calculating the inverse repeatedly.

## Assumptions: matrix is invertible according to the homework instructions

## a function "wrapped" around a list of functions of an invertible matrix.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) m <<- inv
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix
## If inverse already calculated then retrieves the inverse from cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

## Finally, you just need to run cacheSolve(makeCacheMatrix())