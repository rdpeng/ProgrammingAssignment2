## These two functions intend to save time and computing capacity when dealing
## with matrix inversions. 

## The first function needs a matrix object as an input argument. After setting 
## a placeholder, the matrix is copied into the parent environment and the 
## placeholder is reset. The following functions retrieve the matrix, 
## set up and get the inversed matrix. As a last step, all four functions
## are returned within a list to be used by the second function cacheSolve().

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL 
        set <- function(y) {
                x <<- y 
                m <<- NULL 
        }
        get <- function() x 
        setInvMatrix <- function(solve) m <<- solve 
        getInvMatrix <- function() m  
        list(set = set, get = get, 
             setInvMatrix = setInvMatrix,
             getInvMatrix = getInvMatrix)
}


## This function picks up the aforementioned list and makes a quick check 
## whether there is an inverted version of the matrix object stored in the 
## cache. If so, the output is lead b the phrase "getting cached data" followed
## by the inversed matrix. Otherwise, it will do the inversion and then print
## the inversed matrix without any additional text.

cacheSolve <- function(x, ...) {
        m <- x$getInvMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInvMatrix(m)
        m
}