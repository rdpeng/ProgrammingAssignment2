#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x=matrix()) {
        s <- NULL
        set <- function(x) {
                x <<- x
                s <<- NULL
        }
        get <- function() x
        setsol <- function(solve) s <<- solve
        getsol <- function() s
        list(set = set, get = get,
             setsol = setsol,
             getsol = getsol)
}

# Creates a matrix
m <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)

# Create a cache for the matrix
cache_mat <- makeCacheMatrix(m)

# Set matrix in cache
cache_mat$set(m)

# Get the original cache matrix
cache_mat$get()

#-----------------------------------------------------------------------------------

#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        s <- x$getsol()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsol(s)
        s
}

cacheSolve(cache_mat)
