## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invmatrix <- NULL
  get <- function() x
  putinverseintocache <- function(matrix) {
    invmatirx <<- matrix
    print(c('inside put method : ',matrix))
  }
  getinversefromcache <- function() invmatrix
  
  print('makeCacheMatrix called with x ')
  list(get = get, getinversefromcache = getinversefromcache,
       putinverseintocache = putinverseintocache)
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ##first try of calling the inverse function solve
  invmatrixfromcache <- makeCacheMatrix(x)$getinversefromcache()
  print(c('from cache ', invmatrixfromcache))
  
  ##if (is.null(invmatrixfromcache)) {
    ##return(invmatrixfromcache)
  ##}
  
  ##call the solve function to get the inverse
  invmatrix <- solve(x)
  
  ##put the inverse into cache
  print('adding inverse matrix to cache')
  makeCacheMatrix(x)$putinverseintocache(invmatrix)
}
