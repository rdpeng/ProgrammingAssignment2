## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

invmat <- NULL
 ##return matrix  
 getmatrix <- function() x
  
 setmatrix <- function(y)
  {
    x <<- y
    invmat <<- NULL
    
  }
  getinverse <- function() invmat
  setinverse <- function(inv) invmat <<- inv
  list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)
  

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
