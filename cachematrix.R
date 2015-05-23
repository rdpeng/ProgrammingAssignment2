# R Programming Week 2 Assignment 2 WellSys
# To validate:
# m <- makeCacheMatrix()  
# nw<-matrix(1:5, 4, 4)
# nw
#[,1] [,2] [,3] [,4]
#[1,]    1    5    4    3
#[2,]    2    1    5    4
#[3,]    3    2    1    5
#[4,]    4    3    2    1
# m$set(nw)
# cacheSolve(m) 
#
#[,1]        [,2]        [,3]        [,4]
#[1,] -0.17142857  0.02857143  0.02857143  0.25714286
#[2,]  0.21428571 -0.18571429  0.01428571  0.02857143
#[3,]  0.01428571  0.21428571 -0.18571429  0.02857143
#[4,]  0.01428571  0.01428571  0.21428571 -0.17142857

# makeMatrix receives a matrix variable, and sets variables and functions in memory, 
# and returns a list of functions nested within makeMatrix

# Its function is very similary to makeVector of Example: Caching the Mean of a Vector
makeCacheMatrix <- function(x = matrix()) {
  cache_mx <- NULL
  set <- function(y) {
    cache_x <<- y
    cache_mx <<- NULL
  }
  get <- function() cache_x
  set_matrix <- function(inverse) cache_mx <<- inverse
  get_inverse <- function() cache_mx
  list(set = set, get = get,
       set_matrix = set_matrix,
       get_inverse = get_inverse )
}


# cacheSolve returns the inverted form of the submitted matrix.

cacheSolve <- function(x, ...) {
  cache_mx<- x$get_inverse()               
  if(!is.null(cache_mx)) {                    
    message("getting cached data")   
    return(cache_mx)
  }                                        
  startingmatrix <- x$get()                                        
  endingmatrix <- solve(startingmatrix)   
  x$set_matrix(endingmatrix)             
  endingmatrix   
}
