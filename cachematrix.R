## a pair of functions that cache the inverse of a matrix

## create a special matrix that can cashe its inverse

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



## Check if the inverse of a matrix is saved in cache;
## if yes, pull out that inverse matrix
## if not, compute the inverse and print the inverse matrix

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
        
         ## Return a matrix that is the inverse of 'x'
}
