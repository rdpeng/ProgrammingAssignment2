## I am writing two functions:
## The first one creates a matrix-like object which is actually a list containing
## containing four functions to set and get both the matrix and its inverse.
## The second one interacts with a matrix (actually, an object of the former kind)
## and retreives the inverse matrix (without re-calculating it if there's no need to)
## 



## This function creates an object, actually a list, providing four functions to
## set and get both the matrix and its inverse.
## It makes use of two variables (a parameter 'x' and a local variable 'm'), which
## store locally the matrix and the inverse. Four functions are defined, and in the 
## setinverse() function the '<<-' operator is used to modify the makeCacheMatrix local
## variable 'm' (which belongs to the parent environment of the setinverse() environment)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}  


## This function tries to retrieve the inverse matrix from a matrix-representating list
## as created by the former function. It usese the functions contained in the list to retrieve
## the inverse matrix, and it case it is not yet calculated it does the calculations and store
## the result in the list. Anyhow, it ends up returning the inverse as a result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
