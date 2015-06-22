	# There are two functions: 
	# 1.	makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
	# 2.	cacheSolve: This function computes the inverse of the special "matrix" returned bymakeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

# I tired: 
# https://class.coursera.org/rprog-015/forum/thread?thread_id=447
# 
# Here's an easy visual test for inverse. The digits remain the same but the signs switch columns.m <- matrix(c(-1, -2, 1, 1), 2,2)
# x <- makeCacheMatrix(m)
# x$get()
# [,1] [,2]
# [1,]   -1    1
# [2,]   -2    1
# 
# inv <- cacheSolve(x)
# inv
# [,1] [,2]
# [1,]    1   -1
# [2,]    2   -1
# 
# > inv <- cacheSolve(x)
# getting cached data
# > inv
# [,1] [,2]
# [1,]    1   -1
# [2,]    2   -1



## 1 makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setsolve <- function(solve) m <<- solve
  
  getsolve <- function() m
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


#---------------------------------------------------#

## cacheSolve: This function computes the inverse of the special "matrix" returned bymakeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setsolve(m)
  m
  
}
#---------------------------------------------------#


