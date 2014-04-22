## A function called "makeCacheMatrix" creates a list object with four elements. 
## The purpose of this function is to cache the inverse matrix so that if the inverse matrix has already been calculated, the cached value can be retrieved without needing to re-calculate it. 
## Firstly, the function "makeCacheMatrix" sets the default input value to an empty matrix. 
## Secondly, it initializes the variable "i" as a "NULL" to store the cached value of the inverse matrix. Thirdly, we create a function called "set" that takes an input value "y" and sets the value of "x" (the matrix) to be "y". 
## It also sets the value of "i" (the cached inverse) to "NULL" since the inverse will no longer be valid for the new value of "x". 
## Then we create a function called "get", returning the value of "x" (the matrix). 
## Then we make a function "setinverse" used to cache the inverse of the matrix. 
## And we create a function "getinverse" returning the value of "i "(the cached inverse). Forthly, we create a list object with four elements: set, get, setinverse, and getinverse to allow a user to interact with the cached matrix and its inverse through the functions set, get, setinverse, and getinverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
  set <- function(y) {
          x <<- y
          i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This R code defines a function "cacheSolve" that takes a cached matrix object x (created using the "makeCacheMatrix" function) and calculates the inverse of the matrix. 
## Firstly, we define the function "cacheSolve" which takes a cached matrix object "x" and additional arguments that will be passed to the solve function to calculate the inverse matrix. 
## Secondly, we use a construction to avoid recalculating the inverse matrix if it has already been calculated and cached. 
## If the cached value of the inverse is NULL, the function retrieves the original matrix using the "get" function of the "x" object. 
## It then calculates the inverse matrix using the solve function, passing in any additional arguments. 
## The resulting inverse is cached using the "setinverse" function of the "x" object. 
## Finally, the inverse is returned.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
        message("getting cached data")
        return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
