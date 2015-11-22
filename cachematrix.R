##makeCacheMatrix creates a list of functions that allow to set, get matrix and inverse matrix
##cacheSolve calcutates inverse of the matrix and stores it in cache
##this allows to save CPU time by not recalculating inverse if the matrix did not change


##cacheSolve calcutates inverse of the matrix and stores it in cache
##usage: matr <-makeCacheMatrix()
##       matr$set(1:4, 2, 2)
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    #sets the matrix; requires 3 paramaters
    set <- function(y, n_rows, n_cols) {
        x <<- matrix(y, n_rows, n_cols)
        inv <<- NULL
    }
    
    #simply returns the matrix
    get <- function() x
    
    #saves inverse of the matrix to cache
    set_inv <- function(inverse) inv <<- inverse 
    
    #loads inverse of the matrix from cache
    get_inv <- function() inv
    
    list(set = set, get = get,
         set_inv = set_inv,
         get_inv = get_inv)
}

##cacheSolve calcutates inverse of the matrix and stores it in cache
##Usage:       cacheSolve(matr)

cacheSolve <- function(x) {
    inv <- x$get_inv()
    
    #checks if the inverse was already calculated; no need to check if the matrix was changed since "set" function
    #above assigns NULL to "inv" during the change
    if(!is.null(inv)) {
        message("calculation was already performed; cached data:")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data)
    
    x$set_inv(inv)
    inv
}
