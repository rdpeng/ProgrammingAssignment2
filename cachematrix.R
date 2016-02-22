## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  
  inversa <- NULL
  
  set <- function(y = matrix()){
    x <<- y
    inversa <<- NULL
  }
  
  get <- function(){
    x
  }
  
  setinversa <- function(i){
    inversa <<- i
  }
  
  getinversa <- function(){
    inversa
  }
  
  list(set = set, get = get, setinversa = setinversa, getinversa = getinversa)
  
}


## Write a short comment describing this function
##This function above: makeCacheMatrix creates a matrix object that can cache 
##its inverse. 
##makeCacheMatrix is a function which contains 4 functions: set, get, setinversa and getinversa
##get is a function which returns the matrix x stored in the main function.
##set is a function that sets or changes the matrix stored in the main function.
##setinversa and getinversa are functions that store the value of the input matrix into a variable i.
##these 2 functions above store the value of the input in a variable i into the main
##function makeCacheMatrix (setinversa) and returns it (getinversa).
##

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inversa <- x$getinversa()
  if(!is.null(inversa)) {
    message("retreiving cache data")
    return(inversa)
  }
  data <- x$get()
  inversa <- solve(data, ...)
  x$setinversa(inversa)
  inversa
}

#This cacheSolve calculates the inverse of the matrix, input of makeCacheMatrix
##which is returned by makeCacheMatrix above.
##If the inverse has already been computed ,then cacheSolve
#should retrieve the inverse from the cache.
#If the inverse has not been computed, data gets the matrix stored with makeCacheMatrix
## "inversa", calculates the inverse matrix and x$setinversa(inversa) stores it
##in the object inversa in makeCacheMatrix

  