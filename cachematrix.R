## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# 1- set the value of the matrix
# 2- get the value of the matrix
# 3- set the value of the inverse (inv)
# 4- get the value of the inverse (inv)
makeCacheMatrix<- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv

## Creating the list containing the functions
    list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The following function calculates the inverse (inv) of the special "matrix" created with the above function.
## However, it first checks to see if the inverse (inv) has already been calculated. 
## If so, it gets the inverse (inv) from the cache and skips the computation. Otherwise, 
## it calculates the inverse (inv) of the matrix and sets the value of the inverse (inv) in the cache via
## the "setinverse" function.
cacheSolve<- function(x, ...){
  inv<- x$getinverse()
  if(is.matrix(inv)){
    message("getting cached data")
## Return a matrix that is the inverse of 'x'
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix,...)
  x$setinverse(inv)
## Return a matrix that is the inverse of 'x'
  inv
}
