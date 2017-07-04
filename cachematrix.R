## This file contains a pair of functions 
## whose end goal is to cache the inverse of a matrix. 


## This function takes a matrix as an input, and make a special matrix as an output.
## The output consists a list of four functions, 
## that can get (and set) the value of the input matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}




## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, diag(dim(data)[1]), ...)
        x$setinverse(inv)
        inv
}
