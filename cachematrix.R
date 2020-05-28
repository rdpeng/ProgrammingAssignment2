## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {  ## define the argument with default mode of "matrix"
  j <- NULL                                  ## initialize j as NULL; will hold value of matrix inverse 
  set <- function(y){                        ## define the set function to assign new
  x <<- y                                    ## value of matrix in parent environment
  j <<- NULL                                 ## if there is a new matrix, reset j to NULL
  }
  get <- function()x                         ## define the get fucntion - returns value of the matrix argument
  setInverse <- function(inverse) j <<- inverse  ## assigns value of j in parent environment
  getInverse <- function() j                     ## gets the value of inv where called
  list(set = set, get = get,  
  setInverse = setInverse, 
  getInverse = getInverse)
}


## Write a short comment describing this function

 ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
 ## If the inverse has already been calculated (and the matrix has not changed),
 ## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){
  message("getting cached data")
  return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}
