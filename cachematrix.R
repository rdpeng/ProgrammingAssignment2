# --------------------------------------------------------------------- #

# Title: Programming Assignment 2 
# Date: 4 Nov 2017
# By: Valerie Lim (for Coursera)

# --------------------------------------------------------------------- #

## This script allows user to store a cached Matrix, and it's find its 
## Inversed Matrix.
## Usage - initialize a Matrix, than send it to the makeCacheMatrix and 
## save the returned list. Now you can use the list$get() to get the 
## original matrix, and list$getinverse() to get the Inversed.

# (1)

## For a square matrix, caches it, and create the list to enable
## cached retrival of the Matrix and it's inverse at a later stage.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse=getinverse)

}

# (2)

## Gets the cach object (the list that is created in makeCacheMatrix), 
## calculates the Inverse matrix and fills that in the list.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached inverse matrix")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinverse(inv)
    inv
}

# --------------------------------------------------------------------- #
