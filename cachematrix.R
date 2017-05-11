
## makeCacheMatrix functions do

## 1)set the value of the matrix
## 2)get the value of the matrix
## 3)set the value of inverse(solve)
## 4)get the value of inverse(solve)

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                n <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) n <<- inverse
        getinverse <- function() n
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## This function assumes that the matrix is always invertible
## Return a matrix that is the inverse of 'x'
## The following function returns the inverse of the matrix. It first checks if
## the inverse has already been computed. If so, it gets the result and skips the
## computation. If not, it computes the inverse, sets the value in the cache via
## setinverse function

cacheSolve <- function(x, ...) {
        
        inv <- x$getinverse()
        if(!is.null(n)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        n
}
