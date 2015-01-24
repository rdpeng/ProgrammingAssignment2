## I created two functions that let us create a cache of a matrix and its inverse

## This caches a matrix, which allows us to get and set matrix data quickly

makeCacheMatrix <- function(x = matrix()){
        i <- NULL
        ## create a function to save a matrix
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        ## create a function to get the matrix that has been set
        get <- function(){
                x
        }
        ## create a function to solve the matrix
        setinverse <- function(inverse){
                i <<- inverse                
        } 
        ## create a function to get the inverse of the matrix
        getinverse <- function(){
                i
        }
        ## return these functions so we may access them
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## this caches the inverse of a matrix, so that it may be accessed quickly
## with little computation

cacheSolve <- function(x, ...){
        ## get the inverse if it has been set already
        i <- x$getinverse()
        ## if we have a value for the inverse return it
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        ## get the matrix
        matr <- x$get()
        ## solve the matrix, and set a variable as the solution
        i <- solve(matr, ...)
        ## set the inverse
        x$setinverse(i)
        ## return the inverse
        i
}