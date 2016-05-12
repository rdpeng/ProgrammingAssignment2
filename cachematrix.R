## makeCacheMatrix is a method (that acts as a class in other programming languages) that creates a special
## matrix object, with the methods get, set, getinv, setinv. These functions, return the matrix stored, 
## set a new matrix, get the inverse value stored (NULL if it has not been computed), set the inverse matrix
## respectively
## makeCacheMatrix creates a special matrix object that implements four methods for inverse retrieval via caching.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  get_inv <-function() inv
  set_inv <-function(inver) inv<<- inver  
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)

}


## the following method uses the object created via the first function to store and retrieve matrix inverses
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  
  inv <- x$get_inv()
  if(!is.null(inv)){
    return(inv)
  }
  data <-x$get()
  inv <- solve(data)
  x$set_inv(inv)
  inv
}
