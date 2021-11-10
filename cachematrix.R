## makeCacheMatrix() creates a special 'matrix' object that can cache its inverse.
## cacheSolve() computes the inverse of the 'matrix' returned by makeCacheMatrix().
## If the inverse has already been calculated and the 'matrix' has not changed, it
## will retrieve the inverse from the cache directly



makeCacheMatrix <- function(x = matrix()) {  
## this creates a special 'matrix' object that can cache its inverse
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
                ## Use '<<' to assign a value to an object in an environment 
                ## different from the current environment. 
}
        get <- function()x
        setInverse <- function(Inverse) inv <<- Inverse
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
        ## This list returned is used as an input to cacheSolve()
}




cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        ## if inverse has already been calculated
        if (!is.null(inv)) {
                ##get it from the cache and skip the computation.
                message("getting cached data")
                return(inv)
}
        ## Otherwise, calculates the inverse
        mat <- x$get()
        inv <- solve(mat, ...)
        ## sets the value of the Inverse in the cache via setInverse function.
        xsetInverse(inv)
        
        return(inv)
}
