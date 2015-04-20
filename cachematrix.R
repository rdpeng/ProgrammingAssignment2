## basically the makevector example function with changes
## returns a list of 4 functions
##
## for a matrix m:
##   x <- makeCacheMatrix(m)
##
## the functions can be accessed by:
##  x$set()
##  x$get()
##  x$setinverse()  ## used in the cachesolve function
##  x$getinverse()  ## used in the cachesolve function

makeCacheMatrix <- function(x = matrix()) {
  # set inv to null
  inv <- NULL
  
  # function set the matrix x to a new matrix y and reset inv to NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # returns the matrix x
  get <- function() x
  
  # fuction to set inv to inverse
  # inverse is the argument to the setinverse function
  setinverse <- function(inverse) inv <<- inverse
  
  # returns the inverse inv
  getinverse <- function() inv
  
  # returns a vector containing the defined functions
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## basically the cachemean function with some changes
## returns the inverse of the supplied matrix as inv
## if inv already exists it is returned and no calculations are done


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # get inv
  inv <- x$getinverse()
  
  # if inv is no NULL return inv
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # get the matrix and assign it to data
  data <- x$get()
  
  # use the solve function to calculate the inverse of the matrix data
  # and assign the inverse to inv
  inv <- solve(data, ...)
  
  # assign the inverse to inv and return
  x$setinverse(inv)
  inv
}

## tests
## > source('~/coursera/R_programming/assignment2/ProgrammingAssignment2/cachematrix.R')
## > amatrix <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
## > amatrix$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > cacheSolve(amatrix)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > amatrix$getinverse()
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(amatrix)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2))
## > amatrix$getinverse()
## NULL
## > amatrix$get()
##      [,1] [,2]
## [1,]    0   99
## [2,]    5   66
## > cacheSolve(amatrix)
##             [,1] [,2]
## [1,] -0.13333333  0.2
## [2,]  0.01010101  0.0
## > amatrix$getinverse()
##             [,1] [,2]
## [1,] -0.13333333  0.2
## [2,]  0.01010101  0.0
## > cacheSolve(amatrix)
## getting cached data
##             [,1] [,2]
## [1,] -0.13333333  0.2
## [2,]  0.01010101  0.0
## > m <- matrix(c(-1, -2, 1, 1), 2,2)
## > x <- makeCacheMatrix(m)
## > x$get()
##      [,1] [,2]
## [1,]   -1    1
## [2,]   -2    1
## > inv <- cacheSolve(x)
## > inv
##      [,1] [,2]
## [1,]    1   -1
## [2,]    2   -1
## > inv <- cacheSolve(x)
## getting cached data
## > inv
##      [,1] [,2]
## [1,]    1   -1
## [2,]    2   -1
