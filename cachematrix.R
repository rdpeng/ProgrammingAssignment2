## Assignment: Caching the Inverse of a Matrix
## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##                If the inverse has already been calculated (and the matrix has not changed), 
##                then the cachesolve should retrieve the inverse from the cache.


## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    ## Check if s is NULL
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    
    ## Return a matrix that is the inverse of 'x'
    s
}

## test result

#> k<-makeCacheMatrix(x)
#> k$set(matrix(1:4,2,2))
#> k$get()
#[,1] [,2]
#[1,]    1    3
#[2,]    2    4
#> cacheSolve(k)
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
 
