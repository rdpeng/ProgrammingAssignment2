##makeCacheMatrix :this function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
          x <<- y
          inv <<- NULL
      }
      get <- function() x
      setinverse<- function(inverse) inv_x <<-inverse
      getinverse <- function() inv_x
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
 }


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse
##from the cache and skips the computation. 
## Otherwise, it calculates the invese of the data and sets the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
      if (!is.null(inv)) {
          message("getting cached inverse matrix")
          return(inv)
      }
          matrix <- solve(x$get())
          inv <- inverse(matrix,...)
          x$setinverse(inv)
          inv
 }
