## These functions will take a matrix and return the inverse.
## If the inverse has already been calaculated the computation will be skiped
## and the inverse will be get from the cache.

## This first function calculate the inverse of the matrix x. It first set the matrix
## then get the value of the matrix. In the second part it will set the inverse 
## and then get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y){
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinvers <- function(invers) inverse <<- invers
        getinvers <- function() inverse
        list(set = set, get = get,
                setinvers = setinvers,
                getinvers = getinvers)
}


## This function will calculate the inverse of the matrix created with the makeCacheMatrix function.
## But if the inverse has already been calculated it gets the inverse from the cache
## and doesn't go into calculation.


cacheSolve <- function(x, ...) {
        inverse <- x$getinvers()
        if(!is.null(inverse)){
                message ('getting cached data')
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data,...)
        x$setinvers(inverse)
        inverse
        ## Return a matrix that is the inverse of 'x'
}