## Put comments here that give an overall description of what your
## functions do

## This assignment is about solving the inverse of a matrix by caching the 
## result within a lexical scope of a function:  "makeCacheMatrix" and "cacheSolve"
## Write a short comment describing this function
## makeCacheMatrix is a function which creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
   inv = NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve is a function which computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data") 
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
}

## Check codes

## test1 <- matrix(rnorm(25),5,5)
## mat1 <- makeCacheMatrix(test1)
## cacheSolve(mat1)
##            [,1]       [,2]      [,3]       [,4]       [,5]
## [1,] -0.4077978 -0.2349759 -1.357694  0.7995518 -0.1987873
## [2,] -0.7306199 -0.5626231 -1.326931  0.4056633  0.2584435
## [3,]  0.2663761  0.7256591  1.418351 -0.4486788 -0.4289397
## [4,] -2.2626561 -2.7457326 -7.800730  1.3816876  0.3360522
## [5,]  1.9281163  1.2456067  6.538676 -0.8617548 -0.2223809
