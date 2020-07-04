## Put comments here that give an overall description of what your
## functions do
## The cachematrix.R has 2 functions: 1) creates a special vector of functions and 
## 2) gets the inverse of a matrix gotten from function no.1
## Write a short comment describing this function

## Write a short comment describing this function
## This function  defines a matrix as an input and creates 4 functions: set, get, setinv and getinv. 
## This 4 functions are in a list that by naming the elements allow us to use de $ operator to access
## the funcionts by name 
 
makeCacheMatrix <- function(x = matrix()) {
   invierte <- NULL
   set <- function(y) {
     x <<- y
     invierte <<- NULL
   }
   get <- function() x
   setinv <- function(inv) invierte <<- inv
   getinv <- function() invierte
   list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Write a short comment describing this function
## This function completes makeCacheMatrix function. It validates if the variable is not NULL to 
## calculate the inverse by using Solve function 
 
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
   invierte<- x$getinv()
   if(!is.null(invierte)) {
   message("getting matrix data")
     return(invierte)
   }
   matriz <- x$get()
   invierte <- solve(matriz, ...)
   x$setinv(invierte)
   return(invierte)
   
}