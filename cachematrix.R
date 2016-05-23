## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}


## The function makeCacheMatrix takes as input a square invertible matrix, x, and returns a list containing functions to set the matrix, get the matrix, set the inverse and get the inverse. 

makeCacheMatrix <- function(x = matrix()) {
        inverse_mat = NULL
        set = function(y) {
                x <<- y
                inverse_mat <<- NULL
        }
        get = function() x
        setinv = function(inverse) inverse_mat <<- inverse 
        getinv = function() inverse_mat
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## The following function, cacheSolve, computes the inverse of the matrix returned by the function above. If the inverse value is already present in the cache, then it is displayed from the cache memory else, the value is calculated.

cacheSolve <- function(x, ...) {
        inverse_mat = x$getinv()
        
        # if the inverse has already been calculated,get the value from cache
        if (!is.null(inverse_mat)){
                message("getting the value from cache")
                return(inverse_mat)
        }
        
        # otherwise, calculate 
        mat.data = x$get()
        inverse_mat = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(inverse_mat)
        
        return(inverse_mat)
}