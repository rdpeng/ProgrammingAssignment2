## Put comments here that give an overall description of what your
## functions do
## The function stores a matrix X in memory

## Write a short comment describing this function

## makeCacheMatrix stores matrices in memory using scoping rules

makeCacheMatrix <- function(X = matrix()) {
    inverse <- NULL
    set <- function(Y){
        X <<- Y
        inverse <<- NULL
    }
    get <- function() X
    setinverse <- function(Inverse) inverse <<- Inverse
    getinverse <- function() inverse
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function
# Function use corpcor: This package implements a James-Stein-type shrinkage estimator for the covariance matrix, with separate shrinkage for variances and correlations

cacheSolve <- function(X, ...)
{
    if(require("corpcor")){
        print("corpcor is loaded ")
    } else {
        print("installing corpcor")
        install.packages("corpcor")
        if(require(corpcor)){
            print("corpcor was installed succesfully ")
        } else {
            stop("error installing corpcor")
        }
    }
    inverse <- X$getinverse()
    if(!is.null(inverse)){
        message(" the matrix is on memory")
        return(inverse)
    }
    message("inverse is not in memory so the inverse (if exist) is gonna be computed")
    data <- X$get()
    inverse <- pseudoinverse(data, ...)
    X$setinverse(inverse)
    inverse
}

