## Put comments here that give an overall description of what your
## functions do
#cachematrix.r by Angelo Ruelan

## Write a short comment describing this function
#In here, we utilize two function makeCacheMatrix, makeCacheMatrix
#It is composed of set, get, setinv, getinv

makeCacheMatrix <- function(x = matrix()) { #Initiate the program with the default mode of matrix
  k <- NULL #Initializing K as Null
  set <- function(y){
    x <<- y #Function to get Matrix
    k <<- NULL #In a scenario in which a new matrix is created, this function resets k to null
  }
  get <- function() {x} #Defines the get function
  setInverse <- function(inverse) (k <<- inverse) #Delegate the value of K
  getInverse <- function() {k}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## This function is used in order to get the cache data

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



