## This code builds two fucntions that cache the inverse of a matrix

## The first function makeCacheMatrix creates a special matrix object that can cache its inverse  

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL ## initialize the inverse
  
  ## function to set matrix
  set <- function (matrix) {
    x <<- matrix
    inv <<- NULL
  }
  
  ##function to get matrix
  
  get <- function(){
    x
  }
  
  ##function to set inverse of matrix
  setInverse <- function (inverse){
    inv <<- inverse
  }

  ##function to get the inverse of matrix
  getInverse <- function(){
    inv
  }

  list (set = set, get = get, setInverse=setInverse, getInverse=getInverse) ##return list
}


## The second fucntion cacheSolve computed the inverse of the matrix retuend by makeCacheMatrix.
## If the inverse has already been calculated, then cacheSolve will retrive the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  if (!is.null(m)){
    return(m)
  }
  
  data <- x$get() ## get matrix 
  
  m <- solve(data) %*% data ## calculate inverse
  
  x$setInverse(m)
  
  m ## return

}
