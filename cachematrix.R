## Put comments here that give an overall description of what your
## functions do
## The goal for this project is to write two functions, "makeChacheMatrix" 
## and "cacheSolve" that cache the inverse of a matrix

## Write a short comment describing this function
## The makeCacheMatrix function creates a matrix object that can 
## cache the inverse of an invertible square matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## cacheSolve computes the inverse of the matrix returned
## makeCacheMatrix. cacheSolve retrieves the inverse from
## the cache, if the inverse has been calculated and not changed

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

## Example
## jrf <- matrix(rnorm(12),3, 3)
## jrf3 <- makeCacheMatrix(jrf)
## cacheSolve(jrf3)

##      [,1]       [,2]      [,3]
##[1,]  1.6781568 -2.6566990 -1.475541
##[2,] -0.6965278  2.6666763  3.495486
##[3,]  0.3128682 -0.9770416 -3.359365