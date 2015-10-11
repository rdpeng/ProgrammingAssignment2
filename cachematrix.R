## Caching the Inverse of a Matrix
## Two functions to create: a) special matrix. b) its invers & cache the invers   
## to retrive it when need it.
## 
## These functions was created for Programming Assignment 2
## 

##
## makeCacheMatrix(x = matrix())
## Function to create special matrix object that can cache its inverse.
## Created in accordance with the example provided in the assignment (makeVector)
## So, the function is created as list containng fuctions to set/get the value
## of the matrix & set/get the value of the inverse 
## The function created with assumption that the matrix supplied is alowys 
## invertible, so there is no need to test the matrix with det() function.
## Also, the solve() function should be used to compute the inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}




## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
