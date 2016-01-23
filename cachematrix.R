# Programming Assignment 2: Lexical Scoping

# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  # stored the cached inverse and it is set to NULL as nothing is cached initially
  cachedInv <- NULL
  
  # sets the matrix
  set <- function(y) {
    x <<- y
    # as new matrix is being created, so assigning null to cached matrix.
    cachedInv <<- NULL
  }
  
  # retieves the matrix stored in x
  get <- function() {
    x
  }
  
  # cache the argument 
  setinverse <- function(inverse) {
    cachedInv <<- inverse
  }
  
  # get the cached value
  getinverse <- function() {
    cachedInv
  }
  
  # this list is being returned which contains 4 functions.
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve
# the inverse from the cache.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  # getting the cached matrix.
  cachedInv <- x$getinverse()
  
  # returns the inverse amtrix if the cache contains it.
  if(!is.null(cachedInv)) {
    message("getting cached data.")
    return(cachedInv)
  }
  
  # else compute the inverse matrix and store it in the cache.
  data <- x$get()
  cachedInv <- solve(data)
  x$setinverse(cachedInv)
  cachedInv
}




# Example run:

# > x = rbind(c(1, 2), c(3, 4))
# > mtx = makeCacheMatrix(x)
# > m$get()
# [,1]       [,2]
# [1,]  1.0000000 -0.3333333
# [2,] -0.3333333  1.0000000
# > x = rbind(c(1, 2), c(3, 4))
# > mtx = makeCacheMatrix(x)
# > mtx$get()
# [,1] [,2]
# [1,]    1    2
# [2,]    3    4
# 
# # No cache in the first run
# > cacheSolve(mtx)
# [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5
# 
# # Retrieving from the cache in the second run
# > cacheSolve(mtx)
# getting cached data.
# [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5
# > 
