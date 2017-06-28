## In the first function makeCacheMatrix creates a matrix that is a list
## containing below function:

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
        ## Return a list
        Inv <- NULL
        set <- function(y){
                x <<- y
                Inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inverse <<- Inv
        getinv <- function() Inv
        list(set = set, get = get,
             setinv = setinv, getinv = getinv)
}


## The second function calculates the inverse of the matrix created
## with the first function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Inv <- x$getinv()
        if(!is.null(Inv)){
                ## First checks to see if the inverse has already been
                ## caculated, if so, gets the inverse from the cache and
                ## skips the computation
                message("getting cached matrix")
                return(Inv)} else{
                        ## Otherwise, calculates the inverse and sets
                        ## the value of the inverse in the cache via
                        ## the setinv functon
                        dat <- x$get()
                        Inv <- solve(dat, ...)
                        x$setinv(Inv)
                }
        Inv
}
