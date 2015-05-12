# The following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  ## x: a square invertible matrix
 
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


# The following function returns the inverse of the matrix. It first checks if
# the inverse has beed computed and cached. If yes, the it gets the result and skips the
# computation. If not, then it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(x, ...) {
  ## x: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  inv = x$getinv()
  # if the inverse has already been calculated
  if (!is.null(inv)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  data = x$get()
  inv = solve(data, ...)
  
  # sets the value of the inverse in the cache 
  x$setinv(inv)
  return(inv)
}

# Test Result
# --------------
# > set.seed(11)
# > r = rnorm(17)
# > x = matrix(r, nrow=4, ncol=4)
# Warning message:
# In matrix(r, nrow = 4, ncol = 4) :
#  data length [17] is not a sub-multiple or multiple of the number of rows [4]
# > x
#            [,1]       [,2]        [,3]        [,4]
# [1,] -0.59103110  1.1784892 -0.04572296 -1.53829340
# [2,]  0.02659437 -0.9341513 -1.00412058 -0.25556525
# [3,] -1.51655310  1.3236056 -0.82843324 -1.14994503
# [4,] -1.36265335  0.6249178 -0.34835173  0.01232697
# > m = makeCacheMatrix(x)
# > cacheSolve(m)
#          [,1]       [,2]      [,3]       [,4]
# [1,] -1.052573 -0.5110741  1.500636 -1.9574186
# [2,] -1.378069 -1.0401278  2.056274 -1.7110946
# [3,]  1.597477  0.1118965 -2.143660  1.6950684
# [4,] -1.348883 -0.6038086  1.062469 -0.6091912
