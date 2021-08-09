## The following functions were created to take the input of a matrix
## and return the inverse of the matrix

## This function finds a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y)  {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function returns the inverted matrix if it has already been calculated

cacheSolve <- function(x, ...)  {
    m <- x$getinverse()
    if(!is.null(m))  {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    return(m)
}

