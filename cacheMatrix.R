## this code demonstrates the concept of lexical scoping by making use
## of a set of functions that illustrate caching of a mean from a vector.

## The makeCacheMatrix creates an R object that stores a vector and its mean.

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
  set <- function(y) {
   x <<- y
    inv <<- NULL
}
    get <- function() x
   setInverse <- function() inv <- solve(x) 
  getInverse <- function() inv
 list(set = set,
get = get,
 setInverse = setInverse,
  getInverse = getInverse)
}

## the get and getInverse functions retrieve  "inv", "set",        
## "setInverse" and "x" from their enclosing environment.

funs <- makeCacheMatrix()
 funs$set(matrix(1:4, 2))
  funs$get()
  
##        [,1] [,2]
##  [1,]    1    3
##  [2,]    2    4
  
## Return a matrix that is the inverse of 'x'  
  
funs$setInverse()
funs$getInverse()
 
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
