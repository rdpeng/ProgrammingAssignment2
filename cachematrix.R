## The following functions caches the inverse of a matrix and returns the inverse
## from cache when it's identical vs. calculating it again
## Doing so saves time!

## This function creates an augmented matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    ## Exit if the argument is not a matrix
    if (!is.matrix(x)) {
        message("Invalid matrix input")
        return
    }
    
    ## Initialize the inverse
    x_inv <- NULL
    
    ## Initialize the new matrix object
    set <- function(y) {
        x <<- y
        x_inv <<- NULL
    }
    ## Get the new matrix object
    get <- function() x
  
    ## Calculate matrix inverse
    setInverse <- function(x) x_inv <<- x
    
    ## Get the inverse matrix
    getInverse <- function() x_inv
    
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}


## This function computes, caches and returns matrix inverse.
## If the inverse has already been calculated (and the matrix has 
## not changed), then the function retrieves inverse from cache

cacheSolve <- function(x, ...) {
    
    ## Get the matrix and the inverse of 'x'
    mat <- x$get()
    inv <- x$getInverse()
        
    ## Check to see if the inverse already exists
    ## If matrix is unchanged, its matrix product should be the identity matrix
    if(!is.null(inv) && sum(mat %*% inv - diag(nrow(mat))) == 0) {
        message("Getting cached data")
        return(inv)
    }
    
    ## Calculate the inverse, set the cache and return the inverse
    inv <- solve(mat)
    x$setInverse(inv)
    inv
}

## Unit test outputs below ##

## > a <- matrix(1:4, 2, 2)       ## Create an 'invertible' matrix
## > a_mat <- makeCacheMatrix(a)  ## Cache the matrix using makeCacheMatrix
## > solve(a)                     ## Invert the matrix using the solve function
## [,1] [,2]                      ## Output of solve
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(a_mat)            ## Invert the matrix using cacheSolve
## [,1] [,2]                      ## Output of cacheSolve, it's same as above
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(a_mat)            ## Call cacheSolve again; expect result from cache
## Getting cached data            ## Result did come from cache!
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5