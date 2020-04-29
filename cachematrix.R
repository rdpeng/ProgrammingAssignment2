## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
in <- NULL #initialize the inverse
    set <- function(y) { # set the matrix
        x <<- y
        in <<- NULL
    }
    get <- function() x # get the matrix 
    setInverse <- function(inverse) in <<- inverse # set hte inverse of the matrix 
    getInverse <- function() in # get inverse matrix
    list(set = set, #return list 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
in <- x$getInverse()
    if (!is.null(in)) {
        message("getting cached data")
        return(in)
    }
    mat <- x$get()
    in <- solve(mat, ...) # calculate or solve the matrix
    x$setInverse(in)
    in #return the matrix
}
