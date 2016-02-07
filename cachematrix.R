## These two functions are used to cache the inverse of an invertible matrix
## and are part of the assignments in the R-programming course at Coursera

## The makeCacheMatrix-function is called with an invertable matrix as the argument
## it creates a list of functions to set and get the original matrix 'x'
## and to set and get the inverse matrix 'm'
## The function returns a list, which can be used to call the cacheSolve funtion

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function (matrixinput){
        x <<- matrixinput
        m <<- NULL
    }
    get <- function () x
    setinverse <- function (matrixinverse) m <<- matrixinverse
    getinverse <- function () m
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}

## The cacheSolve-function takes the MakeCacheMatrix-list as an argument
## it first checks if the inverse of the matrix has already been calculated (thus in cache)
## otherwise it calculates the inverse and stores it in the cache
## The function returns the inversed matrix along with a message whether it was computed or taken from the cache

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)){
            message("Cashed matrix:")
            return(m)
        }
        matrixinput <- x$get()
        m <- solve(matrixinput, ...) ##returns the inverse of the matrix
        x$setinverse(m)
        message("Computed matrix, now cashed:")
        m
}
