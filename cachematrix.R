## These functions save computation time by caching the inverse of a matrix.

## makeCacheMatrix returns a list of functions used to set or get the matrix,
##         or set or get the value of the inverse.
## Use:
## Run this function once (outside the loop), e.g.
##         mfun <- makeCacheMatrix(), and then mfun$set(x)
##   OR    mfun <- makeCacheMatrix(x)
## x is the matrix of which the inverse needs to be calculated
## mfun is a list of matrix functions relating to the matrix x
## To have the inverse returned, execute cacheSolve(mfun)
## See also cacheSolve

makeCacheMatrix <- function(x = matrix()) {
        matrix_inverse <- NULL
        set <- function(new_matrix) {
                x <<- new_matrix
                matrix_inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(new_inv) matrix_inverse <<- new_inv
        getinverse <- function() matrix_inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve
## makeCacheMatrix must be run before running cacheSolve
##         (see makeCacheMatrix)
## If mfun was the output of makeCacheMatrix, execute 
##         cachSolve(mfun) to have the matrix inverse returned
## Should the origial matrix change to x2, issue mfun$set(x2)
## cachSolve(mfun) will then return the inverse of x2

cacheSolve <- function(mfun,...) {   ## ... more options for solve
        inv <- mfun$getinverse()   
        if(!is.null(inv)) {
                message("Getting cached data ...")
                return(inv)
        }
        original_matrix <- mfun$get()
        inv <- solve(original_matrix,...)   ## ... more options for solve
        mfun$setinverse(inv)
        inv
}
