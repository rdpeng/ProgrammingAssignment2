## This function enable a use of cached Matrix, and it's Inversed Matrix.
## Usage - initialize a Matrix, than send it to the makeCacheMatrix and save the returned list.
## than send the list to the cacheSolve function, to calculate the inverse.
## now you can use the list$get() to get the original matrix, and list$getinverse() to get the Inversed.


## Gets a square inversable matrix, caches it, and create the list to enable cached rerival 
## of the Matrix and it's inverse at a later stage.

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


## gets the cach object (the list that is created in makeCacheMatrix), calculates the Inverse
## and fill it in the list.

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
