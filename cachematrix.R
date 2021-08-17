## The next two functions creates a special "matrix" object that can cache its inverse and computes the 
## inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache

## makeCacheMatrix function creates a special "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()){
  m = NULL 
  #Set the value of the matrix
  set = function(y){
    x <<- y 
    m <<- NULL
  }
  #Get the value of the matrix
  get = function() x
  #Set the inverse of the matrix
  setInverse = function(inverse) m <<- inverse
  #Get the inverse of the matrix
  getInverse = function() m
  list(set = set, get = get, setInverse= setInverse, getInverse = getInverse)
}


## cacheSolve function calculates the inverse of the special "matrix" created with the above function. 

cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  m = x$getInverse()
  #it first checks to see if the inverse has already been calculated. 
  #If so, it gets the inverse from the cache and skips the computation. 
  #Otherwise, it calculates the inverse of the data and sets the inverse in the cache 
  #via the setInverse function.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data = x$get()
  #Return the inverse of the matrix using the solve function
  m = solve(data, ...)
  #Set the inverse
  x$setInverse(m)
  m
}