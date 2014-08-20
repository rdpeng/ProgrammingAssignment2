## The following is a pair of functions that cache the inverse of a matrix. 

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    
    ## sets the inverse matrix, inv, to NULL as a placeholder for a future value
    inv <- NULL 
    
    ## defines a function to set the matrix, x, to a new matrix, y, and resets the inverse, inv, to NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## returns the matrix, x
    get <- function() x
    
    ## sets the inverse, inv, to inverse
    setinv <- function(inverse) {
        inv <<- inverse
    }
    
    ## returns the inverse, inv
    getinv <- function() {
        inv
    }
    
    ## returns a 'special matrix' (which is really a list) containing all of the functions just defined
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)    
    
    
    ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
    ## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
    ## should retrieve the inverse from the cache.
    cacheSolve <- function(x, ...) {
        
        ## gets inverse from the 'special matrix'
        inv <- x$getinv()
        
        ## If inverse has already been calculated, retrieve the inverse, inv, from cashe
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        
        ## Otherwise, calculates the inverse and sets the value of the invere in the cache     
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        
        ## Return a matrix that is the inverse of 'x'
        inv
    }