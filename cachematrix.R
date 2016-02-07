## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    
    m <<- NULL
  }
  get <- function()x
  setinverse <- function(inverse)m <<- inverse
  
  getinverse <- function()m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <-solve(data, ...)
  x$setinverse(m)
  m
}


## Sample run:
# > x = rbind(c(2, 1/2), c(1/2,2))
# > m= makeCacheMatrix(x)
# > m$get()
#      [,1] [,2]
# [1,]  2.0  0.5
# [2,]  0.5  2.0
# > cacheSolve(m)
#          [,1]       [,2]
# [1,]  0.5333333 -0.1333333
# [2,] -0.1333333  0.5333333
# > cacheSolve(m)
# getting cached data
#         [,1]       [,2]
# [1,]  0.5333333 -0.1333333
# [2,] -0.1333333  0.5333333
# > 
