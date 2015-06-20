# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly.

# We are writing the following functions:
 
# makeCacheMatrix: This function creates a special "matrix" object that can
# cache its inverse. 

# cacheSolve: This function computes the inverse of the special "matrix" returned 
# by makeCacheMatrix above. If the inverse has already been calculated (and the 
# matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


# makeCacheMatrix creates a special "matrix", which is really a list containing functions to:

#  1. set the value of the matrix
#  2. get the value of the matrix
#  3. set the value of inverse of the matrix
#  4. get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y) 
  { ## setting the matrix in temporary environment
    x <<- y
    i <<- NULL
  }
  get <- function() x ## getting or just returning the set matrix
  setinverse <- function(solve) inv <<- solve ## solving for the inverse and setting it
  getinverse <- function() inv ## looking for the inverse and getting it
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}

# cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

# ## sample dataset, 2x2 matrix
# c <- matrix(c(1,5,10,41), nrow = 2, ncol = 2)
# cm <- makeCacheMatrix(c)
# cm$get()
#       [,1] [,2]
# [1,]    1   10
# [2,]    5   41
# 
# cacheSolve(cm)
#            [,1]       [,2]
# [1,] -4.5555556  1.1111111
# [2,]  0.5555556 -0.1111111
# 
# cacheSolve(cm)
# getting cached data
# [,1]       [,2]
# [1,] -4.5555556  1.1111111
# [2,]  0.5555556 -0.1111111
# 
# 
# solve(c) ## confirm same as cacheSolve
#            [,1]       [,2]
# [1,] -4.5555556  1.1111111
# [2,]  0.5555556 -0.1111111
# 
# ## sample dataset, 3x3 random integer generated matrix
# d <- matrix(sample(1:100,9), nrow = 3, ncol = 3)
# 
# dm <- makeCacheMatrix(d)
# 
# dm$get() ## this was my random generated matrix
#       [,1] [,2] [,3]
# [1,]   13   30   52
# [2,]   77   28   67
# [3,]   66   57   73
# cacheSolve(dm)
#             [,1]        [,2]         [,3]
# [1,] -0.02428612  0.01059012  0.007580007
# [2,] -0.01640511 -0.03397321  0.042866720
# [3,]  0.03476678  0.01695240 -0.026625802
# 
# cacheSolve(dm)
# getting cached data
#             [,1]        [,2]         [,3]
# [1,] -0.02428612  0.01059012  0.007580007
# [2,] -0.01640511 -0.03397321  0.042866720
# [3,]  0.03476678  0.01695240 -0.026625802
# 
# solve(d)
#             [,1]        [,2]         [,3]
# [1,] -0.02428612  0.01059012  0.007580007
# [2,] -0.01640511 -0.03397321  0.042866720
# [3,]  0.03476678  0.01695240 -0.026625802
