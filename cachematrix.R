## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly. This pair of
## functions cache the inverse of a matrix.


## makeCacheMatric creates a special "matrix" object that can cache its inverse. 
## It contains a function to do the following:
## 1) set the value of the matrix x
## 2) get the value of the matrix x
## 3) set the value of the inverse of x
## 4) get the value of the inverse of x

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
             setinverse = getinverse,
             getinverse = getinverse)
}


## cacheSolve returns the inverse of the specicial "matrix" x, returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache (and skip re-computation).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
