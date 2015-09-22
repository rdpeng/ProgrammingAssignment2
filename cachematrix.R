## These set of function cache a matrix and its inverse

##     This function creates a special "matrix" 
##     object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL       ##initialize matrix to NULL at first call
    
    ##set the matrix to new values and
    ##reset the matrix to NULL because cache no longer valid
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ##return the matrix values
    get <- function() x
    
    ##invert the given matrix
    setinvert <- function(solve) m <<- solve
    
    ##return the inverted matrix
    getinvert <- function() m

    ##return values of the func and make public
    list(set = set, get = get,
         setinvert = setinvert,
         getinvert = getinvert)

}


##      This function computes the inverse of the 
##      special "matrix" returned by makeCacheMatrix above. 
##      If the inverse has already been calculated 
##      (and the matrix has not changed), then the cachesolve 
##      should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            ##get the inverted matrix defined by x
    m <- x$getinvert()

    ##determine if inverted matrix already calc and if not
    ##already invalidated by the set() call and return cached 
    ##inverted matrix
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ##call get to get the matrix
    data <- x$get()
    
    ##invert the matrix
    m <- solve(data, ...)
    
    ##set the inverted matrix so it doesn't recalc
    x$setinvert(m)
    m
}
