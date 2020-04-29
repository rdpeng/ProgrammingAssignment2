## Put comments here that give an overall description of what your
## functions do
############################
#Shreya Mahajan 29.04.2020
#cachematrix.R
#makeChacheMatrix() #store the result into cache
#cacheSolve() #iterates through cache for the result
              #otherwise calculates the result and sores it in cache
############################

## Write a short comment describing this function
###This function finds sthe inverse of matrix and caches the result for further usage
makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  #'set' for assigning new matrix  
  set <- function (y) {
    x <<- y
    mat <<- NULL
  }
  
  #'get' to return the matrix
  get <- function () x

  #'setInverse' to set the cached result
  setInverse <- function(inverse) {
    mat <<- inverse
  }
  
  #'getInverse' to get the cached result
  getInverse <- function() mat
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Write a short comment describing this function
###Call to this function would check the cached data for result 
###otherwise calculate the result here
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #Check the cached data
  mat <- x$getInverse()
  
  #if data is present...return the result
  if(!is.null(mat)){
    message("Getting cached data!")
    return(mat)
  }
  
  #if data is absent
  ##Calculate the result and store in cache for further usage
  data <- x$get()
  mat <- solve(data,...)
  x$setInverse(mat)
  return(mat)
}
