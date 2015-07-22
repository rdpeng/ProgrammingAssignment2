## Put comments here that give an overall description of what your
## functions do

## The below two functions helps calculating the inverse of a given
## Matrix. For any given Square matrix, these functions calculate the
## inverse matrix and caches it at the GlobalEnv context.
## The Inverse Matrix will be fetched from the Cache for same set of
## values. For new set of values, 'solve' function will be 
## called again as the inverse will not be available in Cache.

## Write a short comment describing this function

## The makeCacheMatrix function performs the below actions 
## 1. helps in setting up data for first time call
## 2. initializes the cache ('invmatrix' variable) with NULL value
## 3. stores the inverse matrix with the 'putinverseintocache' function
## 4. retrives the inverse with 'getinversefromcache' function

makeCacheMatrix <- function(x = matrix()) {
  invmatrix <- NULL
  
  set <- function (xx) {
    x <<- xx
    invmatrix <<- NULL
  }
  
  get <- function() x
  
  putinverseintocache <- function(matrix) invmatrix <<- matrix
  getinversefromcache <- function() invmatrix
  
  list(get = get, set = set, 
       getinversefromcache = getinversefromcache,
       putinverseintocache = putinverseintocache)
  
}

## Write a short comment describing this function
## The cacheSolve function performs the below actions 
## 1. checks if the Inverse for the given input is already available
##    in Cache with 'makeCacheMatrix$getinversefromcache'
## 2. if so, returns the Inverse from Cache
## 3. else, it gets the input data by calling 'makeCacheMatrix$get'
## 4. calculates the Inverse by calling 'solve' function
## 5. puts the Inverse calculated from Step #4 by calling 
##    'makeCacheMatrix$putinverseintocache'

cacheSolve <- function(x,...) {
  ## check if already present in cache
  invmatrixfromcache <- x$getinversefromcache()
  
  if (!is.null(invmatrixfromcache)) {
    print('fetching from cache')
    return(invmatrixfromcache)
  }
  ## get the input data
  inputmatrix <- x$get()
  
  ## call the solve function to get the inverse
  newinvmatrix <- solve(inputmatrix,...)
  
  ## put the inverse into cache
  x$putinverseintocache(newinvmatrix)
  print(c('not available in cache.. put into cache by calling solve'))
  return(newinvmatrix)
}

## This is a convinient function for evaluating the special
## matrix function with different sets of data.
## This function can be invoked with a intial set of data
## as input. eg. testInverse(matrix(c(1,1,0,1),2,2)))

## A simple exception handling scenario is added as a final
## test case for demonstration purpose.
testInverse <- function(x) {
  ## call  makeCacheMatrix function
  testMat <- makeCacheMatrix(x)
  
  ## call cacheSolve first time to demonstrate printing inverse
  ## by calling the solve function and to save into Cache
  print('call with new data')
  cacheSolve(testMat)
  
  ## invoke again with same data, to show that, the same inverse
  ## matrix is fetched from cache
  print('call with same data')
  cachedData <- cacheSolve(testMat)
  print(cachedData)
  
  ## call makeCacheMatrix function
  testMat <- makeCacheMatrix(matrix(c(1,2,3,4),2,2))
  
  ## call cacheSolve first time to demonstrate printing inverse
  ## with the builtin 'solve' function and save into Cache
  print('call with another set of data')
  cacheSolve(testMat)
  
  ## invoke again with same data, to show that, the same inverse
  ## matrix is fetched from cache
  print('call with same data')
  cachedData <- cacheSolve(testMat)
  print(cachedData)
  
  ## test a failure case
  testMat <- makeCacheMatrix(matrix(c(1,1,1,1),2,2))
  
  ## catch the exception and print the exception message
  tryCatch(cacheSolve(testMat),error = function(e) {
    print(c('error finding inverse',e))
  })
}