## These functions
## 1. set the value of a matrix
## 2. get the value of a matrix
## 3. set the value of an inverted matrix
## 4. get the value of an inverted matrix
## 5. calculates the value of an inverted matrix
##    or retrieves it from cache if it already exists
##    and returns it.

## This function creates a list that contains functions
## to get and set a matrix and its inversion.

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

## This function solves and returns an ineverted matrix
## or retrieves it from cache if it already exists.

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

# Code to test:
# A = matrix(4:7,nrow=2,ncol=2)
# print(A)
# B <- makeCacheMatrix(A)
# print(B)
# print(cacheSolve(B))
# print(cacheSolve(B))
