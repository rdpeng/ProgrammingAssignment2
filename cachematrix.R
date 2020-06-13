## The following two functions allow to avoid unnecessary re-calculation of the inverse matrix by caching the inverse result with the original matrix in a newly created object.
## The invese matrix calculation is then performed only if no cached inverse matrix is available, which otherwise is returned without any additional computation
#  

## The function makeCachedMatrix initializes a "CachedMatrix" object as a list of four functions:
## - set: read the input matrix (to be inverted), and delete the cached inverse matrix
## - get: return the input matrix
## - setinv: cache the inverse matrix
## - getinv: return the cached inverse matrix (NULL if not available)

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve returns the inverse matrix for an input CacheMatrix object:
## If a cache inverse matrix is available, the function returns that matrix without further computation, otherwise it calculates the inverse matrix and cache the results in the CachedMatrix object

## Please note that, by assignment, the input matrix is assumed to be invertible, so no checks have been put in place in order to verify this condition 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached inverse matrix")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
