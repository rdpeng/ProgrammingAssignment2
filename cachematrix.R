## makeCacheMatrix is a function that creates a matrix that can cache its own inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <-NULL
  set <-function(y){
    x<<-y
    inv<<-NULL
  }

  get <-function() x
  setInverse <-function(inverse) inv <<- inverse
  getInverse <-function() inv
  list(set=set,
       get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}

## cacheSolve is a function that calculates the inverse of matrix created by "makeCacheMatrix" above
## If the inverse has already been calculated and the matrix remains unchanged then it should get the inverse
## from the cahce

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)){
                message("getting cahced data")
                        return(inv)
        }
        mat <-x$get()
        inv <-solve(mat, ...)
        x$setInverse(inv)
        inv
}
