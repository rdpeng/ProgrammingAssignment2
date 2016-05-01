##For the Coursera Course [R programming] (https:/www.coursera.org/course/rprog)
## Assignment 2: Lexical Scoping


## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Create a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

## Create a *square* matrix (because "solve" only handles square matrices)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}


source("C:/Users/32609714/Desktop/Coursera 2016/Assignment 2 Code.R")
my_matrix <- makeCacheMatrix(matrix(c(2,-3,4,1),2,2))
my_matrix$get()
my_matrix$getInverse()
cacheSolve(my_matrix)
