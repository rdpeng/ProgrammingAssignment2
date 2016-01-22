## Objective: Caching the Inverse of a Matrix
## calculate the inversion of Matrix and cache if already exists 
## as it is costly computation if done repeatedly.


## This function creates a special "matrix" object that get the matrix, calculate the inverse of matrix and cache the inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setInv <- function(solve) m <<- solve
        getInv <- function() inv
        list(set = set,
             get = get,
             setInv = setInv,
             getInv = getInv)
}


## This function computes the inverse of the special "matrix" created above.
## If the inverse already calculated then it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInv()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m
}