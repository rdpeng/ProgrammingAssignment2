## The 2 functions in this file can be used to create matrices which internally cache their inverse.
## Creating the cache matrix is achieved by creating a list of functions that make use of lexical 
## scoping to make it appear as if the matrix passed contains its inverse.

## Creating a "special" matrix which is a list of functions to set/get the underlying matrix
## as well as 2 functions to set and get the matrix' inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function calculates the inverse of a matrix as per the following cases:
## A) The matrix passed already contains its inverse, the cached value will be returned and no 
##    calculation takes places
## B) The matrix passed doesn't contain its inverse, it will therefore be calculated and returned

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
