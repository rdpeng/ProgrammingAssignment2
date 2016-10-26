## Put comments here that give an overall description of what your
## functions do

##Week 3 R Programming Assignment - Nina Patrick, Oct 25, 2016

## Write a short comment describing this function
## The point of this function is to create a special matric object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { 
  ##define the argument with matrix function
  inverse <- NULL
  ## set inverse as NULL will hold the value of the matrix's inverse
  set <- function(y) { ##define set as a function to assign the new value of the matrix
    x <<- y  ## this is the value of the matrix in the parent environment
    inverse <<- NULL  ##
  }
  get <- function() x  ## define get as a function which returns the value of the matrix argument
  setinverse <- function(inverse_matrix) inverse <<- inverse_matrix  #assigns value of inverse to the parent environment
  getinverse <- function() inverse  ##gets the value of inverse when called
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)   ## this list is required to refer to functions with the $ operator
}

## Write a short comment describing this function

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inverse <-x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  inverse_matrix <- x$get()
  inverse <- solve(inverse_matrix, ...)
  x$setinverse(inverse)
  inverse
}

