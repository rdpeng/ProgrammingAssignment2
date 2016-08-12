#Calculating the inverse of a matriz is typically a fast operation.
#However, for a very long matrix, it may take too long to compute 
#the inverse matrix, especially if it has to be computed repeatedly (e.g. in a loop).
#If the contents of a marix are not changing, it may make sense 
#to cache the value of its inverse matrix so that when we need it again, 
#it can be looked up in the cache rather than recomputed. 


#This function creates a special "matrix" object 
#that can cache its inverse.

makeCacheMatrix <- function(X = matrix()) {
  M <- NULL
  set <- function(Y) {
    X <<- Y
    M <<- NULL
  }
  get <- function() X
  setinv <- function(solve) M <<- solve
  getinv <- function() M
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

#This function computes the inverse of the 
#special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated 
#(and the matrix has not changed), then the 
#cachesolve should retrieve the inverse from the cache.

# Note: we are assuming that the matrix supplied is always invertible

cacheSolve <- function(X, ...) {
  ## Return a matrix that is the inverse of 'x'
  M <- X$getinv()
  if(!is.null(M)) {
    message("getting cache data")
    return(M)
  }
  data <- X$get()
  M <- solve(data, ...)
  X$setinv(M)
  M
}

## Applying to a matrix created randomly

A <- makeCacheMatrix()
A$set(matrix(c(1,2,3,4,5,6,7,8,1), 3))
cacheSolve(A)

# the second time it gets it from cache
cacheSolve(A)
