## Put comments here that give an overall description of what your
## functions do

#Assignment: Caching the Inverse of a Matrix
#Matrix inversion is usually a costly computation and there may be some benefit
#to caching the inverse of a matrix rather than computing it repeatedly
#This assignment is to write a pair of functions that cache the inverse of a matrix
#The 2 following functions will cache or inverse the matrix:
 
## Write a short comment describing this function
#The first function, makeCacheMatrix creates a special "matrix" object that can cache its inverse
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the inverse of matrix
#4. get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  set <- function(y) {
    x <<- y
    n <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) n <<- inverse
  getinverse <- function() n
  list(set = set, get = get, setinverse = setinverse,getinverse = getinverse)
}


## Write a short comment describing this function

#The second function, cacheSolve computes the inverse of the "matrix" returned by
#makeCacheMatrix. If the inverse has already been calculated (and the matrix has not
#changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  n <- x$getinverse()
  if (!is.null(n)) {
    message("getting cached data")
    return(n)
  }
  data <- x$get()
  n <- solve(data, ...)
  x$setinverse(n)
  n
}

#Testing the code

> source("makeCacheMatrix.R")
> source("cacheSolve.R")
> m <- matrix(rnorm(12),3,3)
> m1 <- makeCacheMatrix(m)
> cacheSolve(m1)
           [,1]     [,2]      [,3]
[1,]  -3.029083 1.982829 -1.562629
[2,]  -7.543878 5.240929 -6.005511
[3,] -11.040582 6.634498 -7.878222
