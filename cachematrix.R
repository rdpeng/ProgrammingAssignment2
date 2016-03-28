## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a matrix with a new atribute call invAux wich is the place where it contains the inverse of the matrix
## at the begginig this atribute is null.
makeCacheMatrix <- function(x = matrix()) {
        invAux <- NULL
        set <- function(y) {
                x <<- y
                invAux <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) invAux <<- inverse
        getInverse <- function() invAux
        list(set = set,
                get = get,
                setInverse = setInverse,
                getInverse = getInverse)
}


## Write a short comment describing this function
## This function recives a matrix created with the function makeCacheMatrix 
## First evaluete if the matix is not null if is not then it returns the inverse of the matrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invAux <- x$getInverse()
        if (!is.null(invAux)) {
                message("getting cached data")
                return(inv)
        }
        matAux <- x$get()
        invAux <- solve(matAux, ...)
        x$setInverse(invAux)
        invAux
}
