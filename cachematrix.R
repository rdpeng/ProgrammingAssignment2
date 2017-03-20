## makeCacheMatrix and cacheSolve take some matrix x invert it and then store it as the variable inv_m such
## that looping functions refer back to the pre-computed inverted matrix rather than performing the inversion
## many times.

## makeCacheMatrix creates the conditions for cacheSolve to return the inverse of some matrix x

makeCacheMatrix <- function(x = matrix()) {
    inv_m <- NULL
    set <- function(y) {
    x <<- y
    inv_m <<- NULL
    }
    get <- function() x
    setsolve <- function(inverse) inv_m <<- inverse
    getsolve <- function () inv_m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

# cacheSolve takes the 'matrix' inv_m passed by makeCacheMatrix(), checks to see if inv_m is NULL and if not
# calculates the inverse of the matrix x.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_m <- x$getsolve()
    if(!is.null(inv_m)) {
          message("getting cached inverted matrix")
          return(inv_m)
    }
    else {
      inv_m <- solve(x$get())
      x$setsolve(inv_m)
      return(inv_m)
    }
  }
