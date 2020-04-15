## Assignment 2 (Week 3) - catching the inverse of a matrix

##makeCacheMatrix: This function creates a special 
##"matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        ##Defining my inv local variable inital value
        inv <- NULL
        
        ##Setting my squared matrix in cache and cleaning 
        ## my inv cache variables
        
        set <- function(y) {
                x <<-y
                inv <<- NULL
        }
        
        ## Defining my desired functions to manipulate the 
        ##inverse of my target matrix, while storing my 
        ##function tools in a list within my environment
        
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


##cacheSolve: This function computes the inverse of the 
##special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the 
##matrix has not changed), then the cachesolve should 
##retrieve the inverse from the cache.

## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        
        ## Environment list manipulation
        ## With my matrix defined, it gets current inv results
        inv <- x$getinv()
        
        
        ##Test cache results existency
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ##if cache is empty run required functions to calculate 
        ##inversed matrix and update the list generated inv val 
        
        data <- x$get()
        
        inv <- solve(data, ...)
        
        x$setinv(inv)
        
        inv
        
}

