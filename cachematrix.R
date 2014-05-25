## Put comments here that give an overall description of what your
## functions do
## I followed very closely class example: Caching the mean of a Vector.
## First funcion makeCacheMatrix is a group of procedures to set a matrix and it's inverse, and also
##  get a matrix and it's inverse

## Write a short comment describing this function
## This funcion includes:
##    set : set the value of matrix
##    get : get the value of matrix
##    setInverse : To calculate inverse of matrix
##    getInverse : To get inverse matrix

makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
## This function calculates inverse of matrix created usin makeCacheMatrix. First check if the inverse has been calculated. If so, uses that, and skip calculations.
## And also return inverse matrix

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}

