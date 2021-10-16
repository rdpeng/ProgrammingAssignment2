## Assignment: Caching the Inverse of a Matrix
less 
Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.

<<- makeCacheMatrix: 
##  This function creates a special "matrix" object that can cache its inverse.
## The list containing: set the value of the vector, get the value of the vector, set the value of the mean, get the value of the mean

Answer:
     makeCacheMatrix <- function(x = matrix()) {               ## Using "matrix" to define the prblem
      inv <- NULL                                              ## inv as null, this hold the value of inverse matrix
     set <- function(y){                                       ## defining the set function
    x <<- y                                                    ## matrix in parent environment
    inv <<- NULL                                               ## inv as null for the new matrix
  }
     get <- function() x                                       ## defining the get function of the matrix
      setInversematrix <- function(solveMatrix) inv <<- solveMatrix            ## assigning the value of inv
      getInversematrix <- function() inv                                       ## gets the value of inv
     list(set = set, get = get, 
         setInversematrix = setInversematrix, getInversematrix = getInversematrix) 
         }


<<- cacheSolve: 
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

Answer: 

     cacheSolve <- function(x, ...) {
    
     inv <- x$getInversematrix()                        ## Return a matrix that is the inverse of 'x'
       if(!is.null(inv)){
       message("getting cached data")
       return(inv)
  }
     data <- x$getinverse()
      inv <- solve(data)
      x$setInversematrix(inv)
     inv      
}

## Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.
## For this assignment, assume that the matrix supplied is always invertible.

