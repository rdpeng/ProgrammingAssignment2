## pair of functions that cache the inverse of a matrix

## this function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <<- NULL
      set <- function(mtrx) {
            x <<- mtrx
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function () inv
      
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Check matrix and get reverse from cache
## if not available, calculate and set reverse to cache

cacheSolve <- function(cm, ...) {
      if (is.null(cm$getinv())) {
            calculate_inver <- solve(cm$get(), ...)
            cm$setinv(calculate_inver)
            cm$getinv()
      }
      else {
            print("Get inverse from cache!")
            cm$getinv()
      }
        
}
