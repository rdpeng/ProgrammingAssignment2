## Put comments here that give an overall description of what your
## functions do
## Two functions that combined solve and store the inverse of a square invertible matrix.

## Write a short comment describing this function
## First the function to set and get the inverse matrices into memory. 

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
        
## Write a short comment describing this function
## Secondly the function that first checks if a matrix 
## inverse has already been stored in memory and, if not
## calculates the inverse of the matrix and stores it into memory.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
        message("getting cached data")
        return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
        ## Returns a matrix that is the inverse of 'x'
}

## To calculate and store the inverse of a matrix use:
## a <- matrix(d, nrow=e)
## b <- makeCacheMatrix(a)
## cacheSolve (b)
## 
## 'd' must be a range of numbers that can fill up a square matrix
## (or when taken the root of the lenght has an integer number as outcome). 
## As the matrix is square, defining the number of rows, will automatically
## also define the number of columns.