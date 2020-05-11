## First function (makeCacheMatrix) creates a special 'matrix' that can cache its inverse: 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
#get/set values for the matrix
        get <- function() x
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
#get and set values for the inverse
        getinv <- function() inv
        setinv <- function(inverse) inv <<- inverse
        
#return a list of functions for the matrix
        list(get=get, set=set, getinv=getinv, setinv=setinv)
}


## This second function (casheSolve) calculates the inverse of the matrix created above. 

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        
        # check to see if inverse has already been calculated. If so, return this. 
        if (!is.null(inv)) {
                message("inverse is cached")
                return(inv)
        }
        
        # computes the inverse of the matrix. 
        m <- x$get()
        inv <- solve(m, ...)
        
        # cache inverse
        x$setinv(inv)
        
        # return inverse of matrix
        inv
}
