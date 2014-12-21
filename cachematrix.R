## Function to get inverse of the matrix.
## If the inverse already exists in the cache, 
## then the result is taken from the cache instead of recalculating.

## This function makes a matrix object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL #Initialize inverse to NULL
        set <- function(y){ #Set the matrix elements
                x <<- y
                inv <<- NULL #In case the matrix is changed previous inverse is forgotten
        }
        get <- function() x #Return the matrix
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv #Returns inverse
        #Return list of functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Function to compute inverse of matrix if the given matrix
## from above doesn't already have its inverse stored in cache

cacheSolve <- function(x, ...) {
        nv <- x$getInverse()
        #Check if inverse in the cache. If true, return result from cache
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        #If the inverse is not present in cache, it calculates and sets the cache
        data <- x$get()
        m <- solve(data, ...)
        x$setmean(m)
        ## Return a matrix that is the inverse of 'x'
        m
}
