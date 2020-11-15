## Put comments here that give an overall description of what your
## functions do

## Coursera - R Programming - Programming Assignment 2: Lexical Scoping
## GitHub user: demi-pgn
## Cache the inverse of a matrix


## Write a short comment describing this function
## The makeCacheMatrix function creates a matrix object that caches its inverse. 
## This matrix object is a list containing a function to set & get the value of the matrix and set & get the inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                             #Initialize m as NULL. The inverse value will be stored in m.
  set <- function(y){
    x <<- y 
    m <<- NULL
  }
  get <- function() x
  setInv <- function() m <<- solve(x)   #Calculate the inverse matrix with solve
  getInv <- function() m                #Get the value of inverse
  list (set = set, get = get,
        setInv = setInv,
        getInv = getInv)
}


## Write a short comment describing this function
## The cacheSolve function computes the inverse of the matrix returned by the makeCacheMatrix function.
## It checks if the inverse matrix is already calculated. If not, it calculates the inverse of the data and sets the result
## in the cache with the setmean function.
cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if (!is.null(m)){
    message("Getting cached inverted matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}
