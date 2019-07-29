## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##create a matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
##set the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
##get the matrix
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
##Return the list of methods      
  list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)


}


## This will retreive the inverse of the matrix

cacheSolve <- function(x, ...) {
       
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mats <- x$get()
        inv <- solve(mats, ...)
        x$setInverse(inv)
        inv
}

