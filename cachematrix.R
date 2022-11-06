setwd('C:/Users/rubind1/Documents/Coursera-R')
##
## I simply set the input x as a matrix
## and then set the solved value "s" as a null
## then I changed every reference to "mean" to "solve"
HEAD
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  
makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3))
  dd69453dc9e8ca087e55e624b0bd3c7c2ec5534f
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
 HEAD
  {
    x<-gs(y)
    x%%y
  }
 dd69453dc9e8ca087e55e624b0bd3c7c2ec5534f
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
##
## Same here, changed "mean" to "solve" and "m" to "s"
cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting inversed matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
