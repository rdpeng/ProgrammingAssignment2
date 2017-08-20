## The two functions below are used to create a special object that stores 
## a numeric matrix and cache's its inverse.

## makeCacheMatrix creates a special "matrix", which is a list containing 
## a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The following function calculates the inverse of the special "matrix" 
## created with the above function. 
## It first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets 
## the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
# Caching the inverse of matrix

### This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  set <- function(y) {
    x <<- y
    invm <<- NULL
  }
  get <- function()x
  setinvmatrix <- function(solve) invm <<- inversematrix
  getinvmatrix <- function() invm
  list(set = set, get = get, setinvmatrix = setinvmatrix, getinvmatrix = getinvmatrix)
}


### This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
### If the inverse has already been calculated (and the matrix has not changed), 
### then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  invm <- x$getinvmatrix()
  if(!is.null(invm)) {
    message("getting inversed matrix")
    return(invm)
  }
  data <- x$get()
  invm <- solve(data,...)
  x$setinvmatrix(invm)
  invm
}
# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
