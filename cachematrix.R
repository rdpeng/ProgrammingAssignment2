+##setwd('C:/Users/erica/Documents/cachematrx.R')

## Write a short comment describing this function
## I simply set the input x as a matrix
## and then set the solved value "s" as a null
## then I changed every reference to "mean" to "solve"
mamakeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

makeCacheMatrix <- function(x = matrix()) {
##  changed "mean" to "solve" and "m" to "s"
cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting inversed matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}

inv <- NULL
set <- funciton(y) {
  x <<-y
  inv << NULL

##Write a short comment describing this function
##makeCacheMatrix is a function which creates a special "matrix" object that can
##cache its inverse for the input (which is an invertible square matrix)
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
get <- function () x
setinverse <- function (inverse) inv <<- inverse.gaussian(
  getinverse <- function() inv
  list(set= set.
       get = get.
       setinverse = setinverse.
       getinverse= getinverse)

##Write a short comment describing this function
##cacheSolve is a function which computes the inverse of the special "matrix"
##returned by makeCacheMatrix above. If the inverse has already been calculated
##(and the matrix has not changed), then the cachesolve should retrieve the
##inverse from the cache
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

## Write a short comment describing this function
##---------------Checking the program------------------------
  m <- matrix(rnorm(16),4,4)
m1 <- makeCacheMatrix(m)
cacheSolve(m1)
##RETURN
##[,1] [,2] [,3] [,4]
##[1,] -0.1653269 0.2592203 0.6176218 -0.7520955
##[2,] 0.2828334 -0.1853499 0.4511382 0.2094365
##[3,] 0.1434840 1.0413868 -0.3550853 -0.3261154
##[4,] 0.1793583 -0.4252171 -0.4371493 -0.1749830

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

##This function creates a special "matrix" object that can cache its inverse.
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

s <- x$getsolve()
if(!is.null(s)) {
  message("getting inversed matrix")
  reurns(s)
  {
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
##This function computes the inverse of the special "matrix" created by
##makeCacheMatrix above. If the inverse has already been calculated (and the
##                                                                  matrix has not changed), then it should retrieve the inverse from the cache.
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

##testing my function

##source("ProgrammingAssignment2/cachematrix.R")
my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_matrix$get()
##RETURN
##[,1] [,2]
##[1,] 1 3
##[2,] 2 4
my_matrix$getInverse()
##RETURN
NULL
cacheSolve(my_matrix)
##RETURN
##[,1] [,2]
##[1,] -2 1.5
##[2,] 1 -0.5
cacheSolve(my_matrix)
##RETURN
##getting cached data
##[,1] [,2]
##[1,] -2 1.5
##[2,] 1 -0.5
my_matrix$getInverse()
##RETURN
##[,1] [,2]
##[1,] -2 1.5
##[2,] 1 -0.5
my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
my_matrix$get()
##RETURN
##[,1] [,2]
##[1,] 2 1
##[2,] 2 4
my_matrix$getInverse()
##RETURN
##NULL
cacheSolve(my_matrix)
##RETURN
##[,1] [,2]
##[1,] 0.6666667 -0.1666667
##[2,] -0.3333333 0.3333333
cacheSolve(my_matrix)
##RETURN
##getting cached data
##[,1] [,2]
##[1,] 0.6666667 -0.1666667
##[2,] -0.3333333 0.3333333
my_matrix$getInverse()
##RETURN
##[,1] [,2]
##[1,] 0.6666667 -0.1666667
##[2,] -0.3333333 0.3333333
