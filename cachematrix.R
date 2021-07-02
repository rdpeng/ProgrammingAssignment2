## Put comments here that give an overall description of what your
## functions do
#cachematrix.r by Angelo Ruelan

## Write a short comment describing this function
#In this code we have the function makeCacheMatrix
#MakeCacheMatrix creates a matrix object  in order to cache its inverse
#It is composed of set, get, setinv, getinv as we see in the code below

makeCacheMatrix <- function(x = matrix()) { #This function is used to initialize the program with the default mode of matrix
  k <- NULL 
  set <- function(y){
    x <<- y 
    k <<- NULL #In a scenario in which a new matrix is created, this function resets k to null
  }
  get <- function() {x} #Defines the get function
  setInverse <- function(inverse) (k <<- inverse) 
  getInverse <- function() {k}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## As mentioned earlier, MakeCacheMatrix gets the inverse of its created matrix
## CacheSolve on the other hand computes this inverse.
## Basically, this is used in order to get the cache data

cacheSolve <- function(x, ...) { #Returns the inverse of X
  k <- x$getInverse()
  if(!is.null(k)){
    message("Getting Cached Data")
    return(k)
  }
  mat <- x$get()
  k <- solve(mat,...)
  x$setInverse(k)
  k
  ## Return a matrix that is the inverse of 'x'
}



