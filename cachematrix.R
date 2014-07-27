##  There are two functions that have been defined below to cache computations of 
## a inverse of a matrix. The first function makeCacheMatrix function
## sets and stores the value of inverse for a matrix. The second function 
## searches for the inverse, if computed already, else computes the inverse of the matrix



## This functions stores the value of the matrix and its inverse of the matrix

makeCacheMatrix <- function(x = matrix()){
  # initializing the inverse variable 'inv' to NULL
  inv <- NULL
  set <- function(y){
    ## this function is used to store the value of matrix in a different environment
    x <<- y
    ## This initiates the inverse and sets it to NULL
    inv <<- NULL
  }
  
  get <- function() x
  # this function is used to obtain the value of the matrix
  
  setinv <- function(solve) inv <<- solve
  # the setinv function uses the inbuilt function 'solve' to determine 
  #the inverse of the matrix x
  
  getinv <- function() inv
  # the getinv function is used to obtain the value of the inverse function
  #of the matrix
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function searches for the inverse of the matrix. If the inverse is found,
## then the value is pulled out from the memory, else it is computed and stored in
## the memory


cacheSolve <- function(x, ...){
  
  m <- x$getinv() 
  # searching the inverse of the function in different environment
  
  if (!is.null(m)){
    message("getting cached data")
    # if the inverse of the function is found then the inverse value will be printed 
    # from the memory
    return(m)    
  }
  data <- x$get()
  # computing the inverse of the matrix using the solve function
  m <- solve(data,...)
  # storing the inverse of the matrix in the memory
  x$setinv(m)
  m
}
