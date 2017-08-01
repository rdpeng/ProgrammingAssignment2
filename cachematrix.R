## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse.
## it supports setting matrix, getting matrix, setting inverse and getting
## inverse

makeCacheMatrix <- function(x = matrix()) {
         invtmatrix <- NULL
                setmatrix <- function(y) {
                        x <<- y
                        invtmatrix <<- NULL
                }
                getmatrix <- function() x                                   #get the value of the Matrix
                setinverse <- function(inverse) invtmatrix <<- inverse      #set the value of the invertible matrix
                getinverse <- function() minvtmatrix                        #get the value of the invertible matrix
                list(setmatrix = setmatrix, getmatrix = getmatrix,
                     setinverse = setinverse,
                     getinverse = getinverse)
      
}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the
## cachesolve will retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invtmatrix <- x$getinverse()
        if(!is.null(invtmatrix)) {                            #if inverse matrix is not NULL
                
                message("getting cached data")                #Type message: Getting Cached Invertible Matrix
                return(invtmatrix)                            #return the invertible matrix
        }
        data <- x$getmatrix()                                 #get the original Matrix Data 
        invtmatrix <- solve(data,...)                         #use solve function to inverse the matrix
        x$setinverse(invtmatrix)                              #set the invertible matrix
       terurn( invtmatrix)                                            #return the invertible matrix

        
}
