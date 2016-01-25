## Technique to avoid recalculating the inverse of a matrix
## The functions will allow us to return the inverse from cache
## if the matrix has not changed


## makeCacheMatrix: Provides the interfaces/methods that implement the technique
makeCacheMatrix <- function(mat = matrix()) {
        mat_inv <- NULL	# local storage for the inverse
        set <- function(y) {
                mat <<- y
                mat_inv <<- NULL
        }
        get <- function() mat
        setInverse <- function(param_inv) mat_inv <<- param_inv
        getInverse <- function() mat_inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve: Retrieve the inverse from cache if it already exists.
##				Calculates inverse if the matrix has changed (or is new)
cacheSolve <- function(mat, ...) {
        ## Return a matrix that is the inverse of 'mat'
        inv <- mat$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        loc_mat <- mat$get()
        inv <- solve(loc_mat, ...)
        mat$setInverse(inv)
        inv
}
