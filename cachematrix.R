## The first function, makeCacheMatrix, creates a matrix that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  
  inv<-NULL
  ##inverse can be doine using solve function. assume x is square invertible matrix.
  
  ## function makeCacheMatrix is a combination of four invidivual functions: set inverse and get inverse.
  ## get will repeat back what it is given (x)
  ## set will set x equal to the matrix from the main function, and stores it in a global space using '<<-'. m<<-NULL reinitializes the result variable.
  ## set inverse will store the result variable, m, from the main function into a global space.
  ## get inverse will retrieve the result from the result variable, m, from the main function.
  
  set<-function(y) {
    x<<-y
    inv<<- NULL
  }
  
  get<-function() x
  
  setinverse<-function(inverse) inv<<-inverse
  
  getinverse<-function() inv
  
  ##this line stores a group of functions into a list for consolidation into a single object
  
  list(set=set, get=get, setinverse = setinverse, getinverse = getinverse)
  
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

## input of the cacheSolve function is the object generated from the list in makeCacheMatrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  #subsetting X since X above contains two different functions: set inverse and get inverse
  
  inv<-x$getinverse()
  
  ##inv%*%x = diag(nrow(inv))
  
  ##verify that matrix has not changed, inverse is found, and inverse is correct?
  if(!is.null(inv)){
    
    message("getting cached data")
    return(inv)
    
  }
  
  data<-x$get()
  
  ##calculate inverse here for square, invertible matrices. If not square or invertible, spit out an error?
  ##assume matrix supplied is always invertible
  inv<-solve(data)
  x$setinverse(inv)
  inv
  
  
}