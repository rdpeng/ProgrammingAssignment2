## Functions defined in this script can set and cache a matrix and its inverse or
## compute the inverse if it does not exist

## This function creates a special matrix object that can cache its inverse
## Examples:
## param "x" is a matrix
# ' matrix_obj <- makeCacheMatrix(matrix(rnorm(3*3), nrow = 3, ncol = 3))
## To change the matrix stored in cache:
# ' matrix_obj$set(matrix(rnorm(2*2), nrow = 2, ncol = 2))

makeCacheMatrix <- function(x = matrix()) {
        ## Return a list of functions (i.e. "special matrix object") that can 
        ## cache and retrieve both a matrix and its inverse
        
        v <- NULL # set matrix inverse to NULL
        
        # function to cache the matrix and matrix inverse to memory
        set <- function(y) {
                x <<- y
                v <<- NULL
        }
        get <- function() x # function to retrieve matrix
        setinv <- function(solve) v <<- solve # function to cache matrix inverse
        getinv <- function() v # function to retrieve matrix inverse
        # Create list of cache and retrieve functions
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function computes the inverse of a matrix. If the inverse of the matrix
## has already been calculated (and the matrix has not changed), then cacheSolve
## will retrieve the inverse from the cache
## Examples:
# ' param 'x' is output from makeCacheMatrix function
# ' matrix_obj <- makeCacheMatrix(matrix(rnorm(3*3), nrow = 3, ncol = 3))
# ' cacheSolve(matrix_obj)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        v <- x$getinv() #call function defined in special matrix object 'x' to 
                        #retrieve inverse of matrix
        
        # Check for matrix inverse and return value if it exists
        if(!is.null(v)) {
                message("getting cached data")
                return(v)
        }
        data <- x$get() #call function in 'x' to retrieve matrix
        v <- solve(data, ...)#solve for matrix inverse
        x$setinv(v) #call function in 'x' to cache matrix inverse
        v 
}
