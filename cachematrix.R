## This file has two functions makeCacheMatrix and cacheSolve
## The purpose of these function is to compute inverse of a 
## matrix once and supply inverse matrix from cache thereafter

## m <- matrix(1:4,2,2)
## m1<-makeCacheMatrix(m)
## cacheSolve(m1)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## This function return a list of functions for a given matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## store matrix and clearcache
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  ## Store inverse in cache
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  ## retun list of functions for matrix x
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## This function first check if inverse matrix exist in 
## Cache. If cache exists then function return inverse 
## from cache otherwise it compute inverse, store it in
## cache and then return inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  ## Check if inverse stored in cache
  if(!is.null(m)) {
    ## in cache
    message("getting cached data")
    return(m)
  }
  ## no compute, store and then return cache
  data <- x$get()
  m <- solve(data,...)
  x$setinv(m)
  m
}

