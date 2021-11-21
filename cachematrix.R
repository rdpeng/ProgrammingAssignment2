## The functions are used to create a special object that stores a matrix and caches its inverse
## The following function creates a special “matrix” by
##setting the value of the matrix

##getting the value of the matrix

#setting the value of the inverse

#getting the value of the inverse

## 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
  set <- function(y) {
          x <<- y
          i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

}


## Write a short comment describing this function
##This function computes the inverse of the special “matrix” returned by makeCacheMatrix above.
##Ifthe inverse has already been calculated (and the matrix has not changed),
##Then CacheSolve should retriev the iverse from the cache
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
        ## Return a matrix that is the inverse of 'x'
}
