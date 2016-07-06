## The purpose of my functions is to catch the inverse of a matrix we set.
## 

## In the first function, makeCacheMatrix creates a special matrix that can cache its inverse. After we 
## set the matrix, it returns a list of the original matrix and the inverse.

makeCacheMatrix <- function(x = matrix()) {
  Inverse = solve(x)
  list(S_matrix = x,
       Inverse = Inverse)
}


## In the second function, it computes the inverse of the special matrix returned by the first function, 
## if the inverse has been calculated, then the cachesolve should retreive the inverse from the cache.

cacheSolve <- function(x, ...) {
        
  m = x$Inverse    ## Return a matrix that is the inverse of 'x'
  if(!is.na(m)){
    message('The inverse has been calculated')
    return(m)
  }
  else{
    inverse = solve(x)
    return(inverse)
  }
}
