# Below is my code for assignment2. It includes the following function components: 
# 1. set the matrix value
# 2. get the matrix value
# 3. set the value for inverse of the matrix
# 4. get the value for inverse of the matrix

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


# below function returns the inverse of the matrix. In the first step, it checks if
# the inverse had already been computed. If so, it directly gets the result and skips
# the computation. Otherwise, it proceeds to compute the inverse anhd set the value in the cache via
# the function.

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

## below I test with a sample:
x = rbind(c(2, -1/6), c(-1/6, 2))
m = makeCacheMatrix(x)
m$get()

##         [,1]       [,2]
##[1,]  2.0000000 -0.1666667
##[2,] -0.1666667  2.0000000

## No cache in the first run
cacheSolve(m)
##           [,1]      [,2]
##[1,] 0.50349650 0.04195804
##[2,] 0.04195804 0.50349650

## Retrieving from the cache in the second run
cacheSolve(m)
## getting cached data.
##           [,1]      [,2]
##[1,] 0.50349650 0.04195804
##[2,] 0.04195804 0.50349650
