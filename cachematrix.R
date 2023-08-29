## This function produces a special “matrix” object that can store or cache its inverse
## if det(A) != 0, then inverse exists

## Notice the superassignment operator <<-
## Used within a lexical scope, where an environment stores the state for a function or set of functions or processes that modify the state by using superassignment.

A <- matrix( c(5, 1, 0,        # Create Sample Matrix A
               3,-1, 2,
               4, 0,-1), nrow=3, byrow=TRUE)

make_Cache_Matrix <- function(x = matrix()) {
     inverse_matrix <- NULL  # set default of inverse_matrix
     get <- function() x  # get the value of the x matrix
     set <- function(y) {  # set the value of the y matrix
          x <<- y          # to the inverse of the x matrix
          inverse_matrix <<- NULL ## SUPERASSIGNMENT OPERATOR
     }
     get_inv <- function() inv
     set_inv <- function(inverse) {
          inverse_matrix <<- inverse  ## SUPERASSIGNMENT OPERATOR
     }
     return(list(
     set = set,
     get = get,
     getinverse = get_inv,
     setinverse = set_inv
     ))
}


## This function produces the inverse of the “matrix” returned by make_Cache_Matrix. If the inverse has already been calculated (and the matrix has not changed), then the cache_Solve should retrieve the inverse from the cache.

cache_Solve <- function(A, ...) {
     ## Return a matrix that is the inverse of 'A'
     inverse <- A$getinverse()     #checks to see if the inverse has already been calculated
          if (!is.null(inverse)) {  #If so, it gets the inverse from the cache
               return(inverse)  #skips the computation
          }
     m_matrix <- solve(A$get()) # use the solve function to calculate the inverse
     A$setinverse(m_matrix)
     ##return(m_matrix)
}
