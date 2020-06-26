##Week 3 Assignment: Caching the Inverse of a Matrix
##makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## following the mean example <<- operator assign a value to an object in an environment that is different from the current environment.


##1. set the value of the matrix

 makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  set <- function(y){
    x <<- y
    n <<- NULL
  }

## 2. get the value of the matrix
  
 get <- function() x
 
## 3. set the value of the inverse
 
  setinverse <- function(inverse) n <<- inverse
  
## 4. get the value of the inverse
  
  getinverse <- function() n
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
 }
 
## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above

 cacheSolve <- function(x, ...) {
   n <- x$getinverse()
   if(!is.null(n)) {
     message("getting cached data")
     return(n)
   }
   data <- x$get()
   n <- solve(data, ...)
   x$setinverse(n)
   n
 }
 
