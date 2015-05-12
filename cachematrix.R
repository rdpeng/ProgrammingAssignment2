## Put comments here that give an overall description of what your
## functions do:
## 2 functions makeCacheMatirx and cacheSolve work to avoid duplicate 
##  calcultation of the same inverted matrix
## example of usage:
## m <- matrix(rnorm(9),3,3)
## x <- makeCacheMatirx(m)  ## called first function 
## y <- cacheSolve(x)       ## called second function - calculate inverted
## y <- cacheSolve(x)       ## called second function ; prints : getting cached data and retruns inverted
## y %*% m                  ## test that this is really identity matrix

## Write a short comment describing this function
## this function create the list of 4 functions
## 1. set - set value of matrix and inverted matrix to NULL (reset)
## 2. get - get matrix to be inverted
## 3. setinv = set inverted matrix to function parameter
## 4. getinv - get value of inverted matrix


makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
    set <- function(y) {
        x <<- y
        xinv <<- NULL

    }
    get <- function() x
    setinv <- function(inv) xinv <<- inv
    getinv <- function() xinv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
## checks if inverted matrix exists 
## if yes - replys getting cached data and returns cached value
## if not calculates inverted matrix and stores it 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    xinv <- x$getinv()
    if(!is.null(xinv)) {
        message("getting cached data")
        return(xinv)
    }
    data <- x$get()
    xinv <- solve(data, ...)
    x$setinv(xinv)
    xinv
}
