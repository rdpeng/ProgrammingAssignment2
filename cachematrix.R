## uses solve() function to calculate inverse of given matrix

## assumed that a matrix which is invertable is given

## this functon creates special matrix which can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## this function calculates the inverse of a matrix. if inverse has already calculate
## and matrix isn't changed, it retrieves the inverse from cache. 

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        } else {
                data <- x$get()
                m <- solve(data, ...)
                x$setsolve(m)
                m
        }
        m
}
