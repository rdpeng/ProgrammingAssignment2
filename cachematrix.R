## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  list(set=set
       , get=get)
}
