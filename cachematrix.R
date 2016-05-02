## The method makeCacheMatrix creates and stores a matrix in memory. By default it creates an empty matrix.
## Then the cacheSolve method is capable of displaying the inverse of the matrix stored in the memory.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(var1 = matrix()) {
      
      var_inverse <- NULL
      
      set_func <- function(var2){
          var1 <<- var2
          var_inverse <<- NULL
      }
      get_func <- function() var1
      set_inverse <- function(set_inverse) var_inverse <<- set_inverse
      get_inverse <- function() var_inverse
      list(set=set_func,get=get_func,setinverse=set_inverse,getinverse=get_inverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(var1, ...)
{
  var2 <- var1$getinverse()
  
  ## Looping to compute the inverse
  ## This loop will only execute if the data is not in the memory. 
  ## If data is present in the memory then the data returned on line 25 will be directly returned.
  if (is.null(var2)) {
    data_in_mem <- var1$get()
    var2 <- solve(data_in_mem, ...)
    var1$setinverse(var2)
  }
  return(var2)
}
