## pair of functions that cache the inverse of a matrix.

##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

## cacheSolve: This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the cache

makeCacheMatrix <- function(x = matrix()) #pass matrix as argument 
  {
  inv <- NULL #set inverse as Null 
  set <- function(y) {
    #define a function to set x to y and reset inv as null 
    x <<- y 
    inv <<- NULL
  }
  get <- function() x #return the matrix
  setInverse <- function(inverse) inv <<- inverse #set the inverse inv to inverse 
  getInverse <- function() inv#return the inv 
  list(set = set,# return all the functions in that list 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}




cacheSolve <- function(x, ...) {
       
  
  inv <- x$getInverse() ## get the inversed matrix from object x
  if (!is.null(inv)) { # if the inversion matrix is there
    message("getting cached data") #print message "getting cached data"
    # return that inverse 
    return(inv)
  }#else (if the inversion matrix not there )
  mat <- x$get() ##get the matrix 
  inv <- solve(mat, ...)##solve matrix
  x$setInverse(inv)##set to the object 
  inv #return result 
  
  
}
