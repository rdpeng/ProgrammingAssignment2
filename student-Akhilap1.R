## This is the R code written for assignment 2 of the R-programming course
## There are 2 functions in this file.
## 1- makeCacheMatrix
## 2- cacheResolve
## Student Akhilap1
## source("https://github.com/Akhilap1/ProgrammingAssignment2/blob/master/cachematrix.R")

###------------------------------------------------------------------------------
##makeCacheMatrix - details
##  input parameters: 1. matrix
##  return: list
##  description: This function takes an object of type matrix and returns a list
##  of functions that get and set Matrix and get and set the MatrixInverse.

makeCacheMatrix <- function(mtx = matrix()) {
    
    mtxinv <- NULL
    
    set <- function(y) 
    {
        mtx <<- y
        mtxinv <<- NULL
    }
    
    get <- function()
    {
        mtx
    } 
    
    setinverse <- function(minverse) 
    {
        mtxinv <<- minverse
    }
    
    getinverse <- function() 
    {
        mtxinv
    }
    
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

###-------------------------------------------------------------
## Return a matrix that is the inverse of 'iMatrixList$get()'
## input : List output from the makeCacheMatrix
## return: Inverse of the Matrix from the input List
## Objective - cache the Inverse once computed and retrun the cached inverse if found

cacheSolve <- function(iMatrixList, ...) {
    
    ##check to see if the matrix inverse already is created
    mInv <- iMatrixList$getinverse()
    
    ##if yes - return the inverse
    if (!is.null(mInv))
    {
        message("getting cached inverse")
        return(mInv)
    }
    
    message("creating inverse")
    
    ##else create inverse, cache it and then return
    m <- iMatrixList$get()
    
    mInv <- solve(m)
    
    iMatrixList$setinverse(mInv)
    
    mInv
}