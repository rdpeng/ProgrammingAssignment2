## These two functions pretend to save time in the computation of the inverse 
## of a matrix when the value has to be computed repeatedly. This is done by
## storing the inverse matrix once calculated and retrieving it from the cache
## in subsequent calls.


## This function creates the object where the matrix is stored.
## Once its inverse is calculated with "cacheSolve", the result is also cached
## in this object.

makeCacheMatrix <- function(x = matrix()) {
    Inv <- NULL
    set <- function(y) {
        x <<- y
        Inv <<- NULL
    }
    get <- function() x
    setInv <- function(solve) Inv <<- solve
    getInv <- function() Inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}



## cacheSolve calculates the inverse of the matrix stored by makeCacheMatrix.
## If the value has already been calculated, the function gets the result from
## the cache. 

cacheSolve <- function(x, ...) { 
    Inv <- x$getInv()
    if(!is.null(Inv)) {
        message("getting cached data")
        return(Inv)
    }
    data <- x$get()
    Inv <- solve(data, ...)
    x$setInv(Inv)
    Inv
}
