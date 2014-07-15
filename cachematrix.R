## The following functions handle the computation and storage of the inverse version
## of a given matrix. 
## The first function computes and stores that inverse matrix in memory.
## The second function checks if the inverse of matrix 'x' has been calculated already
## and, if not, calls the calculation. The inverse of 'x' is being returned.

## Example usage:
## ==============
## amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
## amatrix$get()         # Returns original matrix
## cacheSolve(amatrix)   # Computes, caches, and returns    matrix inverse
## amatrix$getinverse()  # Returns matrix inverse
## cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed matrix inverse
## 
## amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
## cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
## amatrix$get()         # Returns matrix
## amatrix$getinverse()  # Returns matrix inverse

## calculate and store the inverse matrix of the matrix given as parameter 'x'
makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  
  # return the original matrix
  get <- function() x
  # return the inverse matrix
  getinverse <- function() im
  # 'initialise' the stuff
  set <- function(mat) {
    x <<- mat
    im <<- NULL
  }
  # set the inverse matrix
  setinverse <- function(inv) im <<- inv
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## return the inverse matrix of parameter 'x'.
## First of all check if the inverse has been calculated already.
## If yes, return that inverse matrix. 
## Otherwise calculate, cache and return the inverse matrix of 'x'

cacheSolve <- function(x, ...) {
  inversematrix <- x$getinverse()
  # return the inverse matrix if it has been calculated already
  if(!is.null(inversematrix)) {
    message("Inverse matrix has been calculated already, getting cached data.")
    return(inversematrix)
  }
  
  matr <- x$get()
  # calculate the inverse matrix
  inv_m <- solve(matr, ...)
  # store that inverse matrix in the cache
  x$setinverse(inv_m)
  # print that matrix, thereby 'return' it
  inv_m  
  
}
