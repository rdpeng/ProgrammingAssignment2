Assignment: Caching the Inverse of a Matrix
less 
Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.

Write the following functions:

Write the following functions:

<<- makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

   
Answer:
     makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
     set <- function(y){
    x <<- y
    inv <<- NULL
  }
     get <- function() x
      setInversematrix <- function(solveMatrix) inv <<- solveMatrix
      getInversematrix <- function() inv
     list(set = set, get = get, 
         setInversematrix = setInversematrix, getInversematrix = getInversematrix) 
         }


<<- cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

Answer: 
     cacheSolve <- function(x, ...) {
    
         ## Return a matrix that is the inverse of 'x'
     inv <- x$getInversematrix()
       if(!is.null(inv)){
       message("getting cached data")
       return(inv)
  }
     data <- x$getinverse()
      inv <- solve(data)
      x$setInversematrix(inv)
     inv      
}

For this assignment, assume that the matrix supplied is always invertible.

