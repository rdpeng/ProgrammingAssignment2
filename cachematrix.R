## Put comments here that give an overall description of what your
## functions do

## Function returns a list of functions:
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse
## 4. get the inverse
## respectively

makeCacheMatrix <- function(x = matrix()){
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Function attempts an lookup, on failure -> evaluates inverse

cacheSolve <- function(x, ...){
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}