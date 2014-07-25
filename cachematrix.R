## Hello! Please read discription:
##
## 1. First function named makeCacheMatrix is returning a list of a defined cached functions 
## 2. Second function named casheSolve function is returning the invers of a function. This calculation took place only at first run.
## Every next run if function doesn't change (is the same) just gives cached result without timeconsuming calculation.

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
         setinverse = setinverse,getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}

## To check my functions just put in R console:
## testmatrix <-makeCacheMatrix(matrix(1:4,2))  ## creating a matrix
## testmatrix$get()
## testmatrix$getinverse()
## testmatrix$set(matrix(2:5,2))                ## creating a matrix
## cacheSolve(testmatrix)                       ## puting created matrix to cache
## cacheSolve(testmatrix)                       ## geting cached matrix with comment "getting cached data"
