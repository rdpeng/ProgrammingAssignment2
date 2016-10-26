## "M" - the original matrix in the parent environment
## "invM" - the inverse of the matrix "M"

## The first function generates a list of functions
makeCacheMatrix <- function(M = matrix()) {
        invM <- NULL
## set the value of the matrix
        set <- function(y) {
                M <<- y
                invM <<- NULL
        }
## get the value of the matrix
        get <- function() M
## set the inverse of the matrix "M"
        setsolve <- function(solve) invM <<- solve
## get the inverse of the matrix "M"
        getsolve <- function() invM
## returns a list of functions
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## The second function computes the inverse of the matix if needed
cacheSolve <- function(M, ...) {
## get the inverse of the matrix "M"
        invM <- M$getsolve()
## if the inverse is already exists, return it
        if(!is.null(invM)) {
                message("getting cached data")
                return(invM)
        }
## if not -- computes the inverse, cache it and return it
        data <- M$get()
        invM <- cache(data, ...)
        M$setsolve(invM)
        invM
}
