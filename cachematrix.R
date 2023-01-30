## a pair of functions that cache the inverse of a matrix
##create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function(){x}
  setinverse <- function(inv){inverse <<- inv}
  getinverse <- function(){inverse}
  list(set = set,get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## compute inverse of the "matrix" above
## If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <-solve(data,...)
  x$setinverse(inverse)
  inverse
}

##take matrix(1:4, 2, 2) and matrix(c(2, 2, 1, 4), 2, 2) as an example
