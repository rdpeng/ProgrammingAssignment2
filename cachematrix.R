## This pair of functions are used to store a matrix and cache it's inverse.

## The makeCacheMatrix takes a matrix as input (so we expect it to be) 
## and returns a list of functions which:
## 1. set the values of the matix
## 2. get the values of the matrix
## 3. set the values of the inverse matrix
## 4. get the values of the inverse matrix

makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve takes the special matrix (object) constructed from the above 
## function and checks if the inverse has already been cached/computed
## if so then this function simply pulls the inverse out of the cache. 
## Otherwise, the function computes the inverse and caches it for the possbility
## of future usage.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of x
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
