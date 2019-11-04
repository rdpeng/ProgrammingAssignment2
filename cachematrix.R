#Caching the Inverse of the Matrix
#The following function used to create special objects that stores the matrix and cache its inverse

#The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,get = get,setInverse = setInverse,getInverse = getInverse)
}


#This cacheSolve function computes the inverse of the special "matrix" created by makeCacheMatrix  
#If the inverse has already been calculated then it retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

###########output execution#################
myMatrix <- makeCacheMatrix(matrix(1:4, 2, 2))  #matrix input
myMatrix$get()                                  #matrix values of myMatrix
myMatrix$getInverse()                           #getting Inverse Matrix before calling cacheSolve
cacheSolve(myMatrix)                            #Find inverse of "myMatrix" created using makeCacheMatrix using cacheSolve
cacheSolve(myMatrix)                            #getting cached data
myMatrix$getInverse()                           #getting Inverse Matrix after calling cacheSolve
############################################
