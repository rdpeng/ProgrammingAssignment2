## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a 'matrix' object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ##***********set the value of the vector***********
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  ##************get value of vector***********
  get <- function() x
  ##************set value of inverse***********##
  setinv <- function(inverse) inv <<- inverse
  ##***********Get value of inverse************##
  getinv <- function() inv
  list(set = set, 
       get = get, 
       setinv = setinv, 
       getinv = getinv)
}



## This function computes the inverse of the special 'matrix' created aove
## - makeCacheMatrix. When the inverse has been computed and the matrix
## remained the same without changing, then the cacheSolve matrix should retrieve the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Return a matrix that is the inverse of x
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}

