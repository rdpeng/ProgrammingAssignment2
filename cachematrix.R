## Since calculating an inverse of a matrix takes some time and resources, we need to create a pair of functions
## which allow to retrieve the resiult of a calcualtion if the calcualtion was already done
## without re-calculating. If inverse of a matrix was not yet calculated, the function will calculate it.
##However, if the result is known, when called again the function will fetch the result from cache.

## makeCacheMatrix function is a construction function which output are four other functions.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}




## cacheSolve is a function which gets x (a matrix) as an input and caluclates its inverse. However,
##it stores the result in cache and if it id called again with the same input, it will just print the 
## previousely calcualted result.

cacheSolve <- function(x, ...) {
                m <- x$getinverse()
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                data <- x$get()
                m <- solve(data, ...)
                x$setinverse(m)
                m
        }
        ## Return a matrix that is the inverse of 'x'
