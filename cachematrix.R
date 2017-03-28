## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        ## set() allows to change the value of the matrix stored in the
        ## makeCacheMatrix environment. When doing so, we clear the value of the
        ## inverse that was previously cached
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## get() returns the value of the matrix stored
        get <- function() x
        
        ##setinv() will be called in the cacheSolve function to store the value
        ## of the inverse calculated there
        setinv <- function(inverse) inv <<- inverse
        
        ## getinv() retrieves the value of the inverse that is stored in
        ## the environment
        getinv <- function() inv
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Let's get the value of inv cached in the makeCacheMatrix's object
        ## environment
        inv <- x$getinv()
        
        ## If inv is not null, i.e. if cacheSolve has run at least once for
        ## this matrix, let's retrieve the cached value
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## If cacheSolve runs for the first time, let's calculate the inverse ...
        data <- x$get()
        inv <- solve(data, ...)
        
        ## ... and cache it in the makeCacheMatrix's object environment
        x$setinv(inv)
        
        ##return the inverse
        inv
}
