## This functoins creates a matrix with length 0, give its gettings and settings.
## Then makes an inverse, if it has not already made.

## This function takes numeric square matrix as a parameter, gives back its gettings and settings.

makeCacheMatrix <- function(x = matrix(numeric(0), 0,0)) {
  k <- NULL
  set <- function(y) {
    x <<- y
    k <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() k
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes output of the first one as a parameter and makes inversion, if it has not already made.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        k <- x$getinverse()
        if(length(k) > 0) {
                message("getting cached data")
                return(k)
        }
        data <- x$get(x)
        k <- solve(data, ...)
        x$setinverse(k)
        k
}
