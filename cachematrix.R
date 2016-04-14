## Two functions I created below are used to 
## create a special "matrix" object (a list containing funtions) that can cache its inverse (makeCacheMatrix)
## and compute the inverse of the special "matrix" returned by makeCacheMatrix (cacheSolve).
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## One may avoid repeatedly time-consumping computing of the same inverse matrix by using these function to cache the result.

## "makeCacheMatrix" is a function to create a list of 4 objects (functions) which are 
## 1. set: to set the value of the matrix
## 2. get: to get the value of the input matrix 
## 3. setinv: to set the value of the returned matrix (the inverse of input matrix) 
## 4. getinv: to get the value of the returned martix (the inverse of input matrix) 


makeCacheMatrix <- function(x = matrix()) {
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


## The function "cacheSolve" is to compute the inverse of the input matrix and determine if the inverse has already been calculated.
## If in this case (been calculated), the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
        ## Return a matrix that is the inverse of 'x'
}

