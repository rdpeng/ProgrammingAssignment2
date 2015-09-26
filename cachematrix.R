## Put comments here that give an overall description of what your
## functions do

## cashed matrix - the cached values

makeCacheMatrix <- function(x = matrix()) {
  cachemx <- NULL
  set <-function (y) {
    x<<-y
    cachemx <<- NULL
  }
  get <- function () x
    setInverse <-function(inverse) cachemx <<-inverse
    getInverse <-function () cachemx
    list (set = set,
          get = get, 
          setInverse = setInverse,
          getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cachemx <- x$getInverse()
  
  if (!is.null(cachemx)) {
    message ("getting cached data")
    return (cachemx)
    }
  matrix <-x$get()
  cachemx <-solve(matrix, ...)
  x$setInverse(cachemx)
  cachemx
 }
    
  
  
##> source("cachematrix.R")
##> set.seed(2)
##> mx <-makeCacheMatrix (matrix(rnorm(4),2,2))
##> mx$get()
##[,1]      [,2]
##[1,] -0.8969145  1.587845
##[2,]  0.1848492 -1.130376
##  > mx$getInverse()
##NULL
##> cacheSolve(mx)
##[,1]      [,2]
##[1,] -1.5692285 -2.204304
##[2,] -0.2566143 -1.245129
##> mx$getInverse()
##[,1]      [,2]
##[1,] -1.5692285 -2.204304
##[2,] -0.2566143 -1.245129
##> cacheSolve(mx)
##getting cached data
##[,1]      [,2]
##[1,] -1.5692285 -2.204304
##[2,] -0.2566143 -1.245129
##> mx$getInverse()
##[,1]      [,2]
##1,] -1.5692285 -2.204304
##[2,] -0.2566143 -1.245129

##> mx <-makeCacheMatrix (matrix(1:9,3,3))
##> mx$get()
##[,1] [,2] [,3]
##[1,]    1    4    7
##[2,]    2    5    8
##[3,]    3    6    9
##> mx$getInverse()
##NULL
##> cacheSolve(mx)
##Error in solve.default(matrix, ...) : 
##  Lapack routine dgesv: system is exactly singular: U[3,3] = 0
##> 
## http://stackoverflow.com/questions/20529062/using-mlogit-in-r-with-variables-that-only-apply-to-certain-alternatives
