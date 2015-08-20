## Caching the Inverse of a Matrix
## makeCacheMatirx
## cacheSolve


## makeCacheMatrix creates a special "matrix", a list containing a function to
## 1. get the value of the matrix
## 2. set the value of the matrix
## 3. get the inverse of the matrix
## 4. set the inverse of the matirx

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


## cacheSolve calculates the inverse of the special "matrix" created with makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
