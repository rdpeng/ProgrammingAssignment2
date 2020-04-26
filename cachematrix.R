## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## A Matrix x is created using makeCacheMatrix function

makeCacheMatrix <- function(x = matrix()) {
     m_inv <- NULL
     set <- function(y) {
          x <<- y
          m_inv <<- NULL
     }
     get <- function() x
     setinverse <- function(m_inverse) m_inv <<- m_inverse
     getinverse <- function() m_inv
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## Write a short comment describing this function
## Inverse of matrix x is found using cacheSolve function

cacheSolve <- function(x, ...) {
 m_inv <- x$getinverse()
     if(!is.null(m_inv)) {
          message("getting cached data")
          return(m_inv)
     }
     data <- x$get()
     m_inv <- solve(data, ...)
     x$setinverse(m_inv)
     m_inv

        ## Return a matrix that is the inverse of 'x'
}
