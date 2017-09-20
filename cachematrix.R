## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than computing it repeatedly

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## Output:

##> x = rbind(c(2, -1/4), c(-2/4, 1))
##> x
##[,1]  [,2]
##[1,]  2.0 -0.25
##[2,] -0.5  1.00
##> m = makeCacheMatrix(x)
##> m$get()
##     [,1]  [,2]
##[1,]  2.0 -0.25
##[2,] -0.5  1.00

##No cache on this run

##> cacheSolve(m)
##[,1]      [,2]
##[1,] 0.5333333 0.1333333
##[2,] 0.2666667 1.0666667

##Using cacheSolve again to retrive the cached data

##cacheSolve(m)

##getting cached data.

##[,1]      [,2]
##[1,] 0.5333333 0.1333333
##[2,] 0.2666667 1.0666667