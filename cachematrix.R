## I have created two function, the first function is consisting of 4 underlying
## functions that creates a matrix, retrieves the matrix, set the inverse of the matrix,
## retrieves the inverse of a matrix and saves all these values to a list.
## The second function retrieves the value of the inverse of the matrix,
## if the inverse haven't been calculated it instead calculate the inverse and 
## saves it. 

## This functions consists of 4 underlying functions set(), get(), setInverse(),
## and getInverse() that respectively sets the values of a matrix, retrieves the
## values of that matrix, sets the inverse of the matrix, and retrieves the inverse
## of the matrix. In the end the function saves all the values of these functions
## to a list. 

makeCacheMatrix <- function(x = matrix()) {
          inv <- NULL
          set <- function(y){
                    x <<- y
                    inv <<- NULL
          }
          get <- function() {x}
          setInverse <- function(inverse) {inv <<- inverse}
          getInverse <- function() {inv}
          list(set = set, get = get, 
               setInverse = setInverse,
               getInverse = getInverse)

}


## This functions retrieves the value of getInverse() and saves
## it to the variable inv.
## If getInverse() is not equal to null, it simply returns the value. 
## If getInverse is equal to null it instead calculates the inverse of the matrix
## saves the result by using setInverse() and returns the result.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                  message("getting cached data")
                  return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}


