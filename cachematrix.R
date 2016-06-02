## This code creates two functions: makeCacheMatrix and cacheSolve. The purpose is to
## perform, store, and retrieve a potentially time-consuming matrix inverse calculation

## The function makeCacheMatrix creates a list containing the following functions:
## 1. set the value of a matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
get <- function() x
setinverse <- function(matinverse) inv <<- matinverse
getinverse <- function() inv
list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Given a square matrix, the function cacheSolve checks to see if the
## inverse has already been calculated. If not, the function calculates
## the inverse and stores the value using the setinverse function defined
## in makeCacheMatrix. If the inverse is already stored, then cacheSolve
## retrieves the inverse and prints the value.

cacheSolve <- function(x, ...){
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    matx <- x$get()
    inv <- solve(matx)
    x$setinverse(inv)
    inv
}

## test run:
x <- rbind(c(1,2), c(3,4))
cm <- makeCacheMatrix(x)
cm$get()

# no chace for first run
cacheSolve(cm)

# retrieve from cache in second run
cacheSolve(cm)
