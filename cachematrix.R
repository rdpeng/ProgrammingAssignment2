
## Write a short comment describing this function
## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" created by the makeCacheMatrix above.
  cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  	  inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
}
