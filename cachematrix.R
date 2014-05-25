
##This function makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ##mat_inv <- NULL                           ## if not already set , initialized with null
        set <- function(y) {
                x <<- y                           ## set x with y( y defined in parent )  
                mat_inv <<- NULL
        }
        get <- function() x                       ## returns the matrix
        setinv <- function(inv) mat_inv <<- inv   ## find the inverse and sets the value of mat_inv
        getinv <- function() mat_inv              ## returning the cached inverse
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


cacheSolve<- function(x, ...) {
        mat_inv <- x$getinv()
        if(!is.null(mat_inv)) {                         ## checks if inverse is already calculated , if yes
                message("getting cached data")          ## prints message
                return(mat_inv)                         ## returns cache data 
        }
        data <- x$get()                                 ## else gets the matrix
        mat_inv <- solve(data)                          ## finds invers
        x$setinv(mat_inv)                               ## sets the new inverse
        return(mat_inv)                                 ## returns inverse
}
