## Create a special "matrix" object that can be cache its inverse
## Create a special matrix, which is really a list containinf a functions to
## 1. set the value of the matrix setm
## 2. get the value of the matrix getm
## 3. set the value of the inverse matrix setmx
## 4. get the value of the inverse matrix getmx

## m1 is a square matrix m1i inverse matrix

makeCacheMatrix <- function(m1=matrix()) {
    m1i <- NULL
    setm <- function(y) {
        m1 <<- y
        m1i <<- NULL
    }
    getm  <- function() m1
    setmx <- function(solve) m1i <<- solve
    getmx <- function() m1i
    list(setm=setm, getm=getm, setmx=setmx, getmx=getmx)
}

## Computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has alrady been calculated (an the matrix has not changed), then the 
## cacheSolve retrieve the inverse from the cache

cacheSolve <- function(m1, ...) {
    m1i <- m1$getmx()
    if(!is.null(m1i)) {
        message("getting cached data")
        return(m1i)
    }
    data <- m1$getm()
    m1i <- solve(data, ...)
    m1$setmx(m1i)
    m1i
}
