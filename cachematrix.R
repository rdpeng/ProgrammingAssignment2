## makeCacheMatrix fuction will
## 1)set the value of the matrix
## 2)get the value of the matrix
## 3)set the value of inverse(solve)
## 4)get the value of inverse(solve)
makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## calculates the inverse of the special "matrix" 
## This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        ## The following function returns the inverse of the matrix. It first checks if
        ## the inverse has already been computed. If so, it gets the result and skips the
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## computation. If not, it computes the inverse, sets the value in the cache via
        ## setinverse function.
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
