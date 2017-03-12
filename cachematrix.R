## Inverse of Matrix is computationally heavy. The below functions
## create a special type of matrix object which can cache the inverse
## of the matrix created using makeCacheMatrix function. Caching removes 
## the need for calculating inverse again for already calculated matrix.
## The makeCacheMatrix functin creates the special matrix object with
## 4 functions(set,get,setinverse,getinverse) for setting/getting the
## matrix and setting/getting the inverse of the matrix.
## The cacheSolve fuction returns the inverse of matrix. 

## makeCacheMatrix function creates a special "matrix" object
## which can cache its inverse. This object has 4 methods, set, get, setinverse
## and getinverse
makeCacheMatrix <- function(x = matrix()) {
  ## set the inverse to null when a new makeCacheMatrix object is created
  inverse <- NULL
  ## If a new matrix is passed in the set function, replace the existing
  ##  matrix in and inverse to null
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  ## get function returns the existing matrix
  get <- function() x
  ## If already calculated inverse is passed in setinverse function
  ## then replace the cached inverse       
  setinverse <- function(matrixinverse) inverse <<- matrixinverse
  ## getinverse function returns the inverse of the matrix
  getinverse <- function() inverse
  ## List of functions available on the matrix created using
  ## makeCacheMatrix
  list(set = set, get = get,
       setinverse = setinverse ,
       getinverse = getinverse )
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Get the existing inverse of the matrix
  inverse <- x$getinverse ()
  ## Check if the inverse is not null, if it is not null
  ## then return the cached inverse
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  ## Get the matrix
  data <- x$get()
  ## find the inverse of the matrix. Solve function gets 
  ## the inverse
  inverse <- solve(data, ...)
  ## Use setinverse function to cache inverse of matrix
  x$setinverse (inverse)
  ## Return a matrix that is the inverse of 'x'
  
}
