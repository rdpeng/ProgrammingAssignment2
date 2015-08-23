## makeCacheMatrix shall create a matrix oject that can inverse itself

makeCacheMatrix <- function(x = matrix()) {

  inv = NULL
  set = function(y) {
    
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)

}


##cachesolve shall check if the the inverse for the matrix has already been
# calculated if so then it shall return from cache
# else shall calculate the inverse using the solve function
# and sets it in the cache using setinv function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
      message("fetching inverted matrix")
    return(inv)
  }
   
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache
  x$setinv(inv)
  
  return(inv)
}
