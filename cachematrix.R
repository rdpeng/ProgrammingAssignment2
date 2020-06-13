
## Put comments here that give an overall description of what your
## functions do

## About makeCacheMatrix function

## The makeCacheMatrix function here does the following job:

## 1: Sets the values in the matrix.
## 2: Gets the values of the matrix,
## 3: Sets the values of the inverse of the matrix.
## 4: Gets the values of the inverse of the matrix.




## About cacheSolve function

## This function calculates the inverse of the Special matrix created 
## with the makeCacheMatrix function.However, it first checks to see if the 
## inverse has already been calculated. If so, it gets the inverse
## from the cache and skips the computation. Otherwise,
## it calculates the inverse of the matrix and sets the
## value of the inverse in the cache via the setInverse function.




## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
       c<-NULL
     setmatrix<-function(y){
       x<<-y
       c<<-NULL
       }
     getmatrix<-function()x
     setInverse<-function(solve) c<<-solve
     getInverse<-function()c
     list(setmatrix=setmatrix,getmatrix=getmatrix,setInverse=setInverse,getInverse=getInverse)
     }




## Write a short comment describing this function

cacheSolve <- function(x, ...) {
     c<-x$getInverse()
     if(!is.null(c)){
       message("Getting Cached Data")
       return(c)
       }
     result<-x$getmatrix()
     c<-solve(result,...)
     x$setInverse(c)
     c
     }
        ## Return a matrix that is the inverse of 'x'
