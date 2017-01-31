## The makeCacheMatrix function takes care of setting and getting Matrices as well as caching and returning them

makeCacheMatrix <- function(x = numeric()) {
    
    # Initializes cache variable with null value
    cache <- NULL
            
    # Sets a matrix
    setMatrix <- function(mat) {
    x <<- mat
    # As the matrix has a new value, the cache variable gets flushed
    cache <<- NULL
    }
  
    # Returns the stored matrix
    getMatrix <- function() {
    return (x)
    }
       
    # Caches the argument as value 
    cacheInverse <- function(solve) {
    cache <<- solve
    }
          
    # Gets the cached value
    getInverse <- function() {
    cache
    }
                
    # Returns the list of created functions
    ist(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)

}




## The cacheSolve function takes care of inversing the matrices

cacheSolve <- function(x, ...) {
  
  # Gets the cached value
  inversedMat <- x$getInverse()
  # Checks if the value is cached and in case of existence, returns that 
  if(!is.null(inversedMat)) {
  message("getting cached data")
  return(inversedMat)
  }
  # If cache is empty, inverses the matrix and fills the cache
  
  matr <- x$getMatrix()
  inversedMat <- solve(matr)
  x$cacheInverse(inversedMat)
         
  # returns the inversed Matrix
  return(inversedMat)
}


