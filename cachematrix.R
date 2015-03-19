##cacheSolve and makeCacheMatrix, creates and then computes the inverse of that created matrix. 
##if the inverse of matrix is already calculated, calculation is not re-done and that 
##pre-calculated inverse is retrieved from the cached value.

## below function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix contains a list of 4 functions to set and get the value
## of a matrix and its calculated inverse. 

makeCacheMatrix <- function(x = matrix()) {
    ##initialization
    inverse <- NULL
    
    ##set the matrix  
    setmatrix <- function(set) {
        x <<- set
        inverse <<- NULL
    }
    
    ##get the matrix
    getmatrix <- function() x
    
    ##set the inverse matrix
    setinvmatrix <- function(inv) inverse <<- inv
    
    ##get the inverse matrix
    getinvmatrix <- function() inverse
    
    ##functions listed    
    list(setm = setmatrix, getm = getmatrix, setim = setinvmatrix, getim = getinvmatrix)
}


##Below function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {

## Checking whether inverse is already calculated or not,
    ##if yes cached calculation is retrieved
    inverse <- x$getim()
    if(!is.null(inverse)) {
        message("getting cached pre-calculated inverse..")
        return(inverse)
    }
    
## inverse is not calculated before,so calculation must be done
    #matrix obtained
    matrix <- x$getm()
    #inverse is calculated
    inverse <- solve(matrix, ...)
    #inverse is cached
    x$setim(inverse)
    inverse
}
