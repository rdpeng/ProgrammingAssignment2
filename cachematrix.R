# Matrix inversion is usually computationally costly
# Caching the inverse matrix rather than computing repeatedly may be a benefit.
# Assumption: Matrix is invertible
# -------------------------------------------------------
# The following two functions cache the inverse of a matrix
# -------------------------------------------------------
# makeCacheMatrix creates a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix
# -------------------------------------------------------

makeCacheMatrix <- function(x = matrix()){
        inv <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# -------------------------------------------------------
# The following function returns the inverse of the matrix.
# First, the matirx inverse is checked if already computed.
# If yes, it gets the results and skips the computation
# If no, it computes the inversem sets the value in the cache with the setinverse function
# -------------------------------------------------------
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(inv)
        inv
}
# -------------------------------------------------------