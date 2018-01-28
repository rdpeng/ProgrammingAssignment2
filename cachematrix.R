## The following pair of functions cache the inverse of a matrix.

## 1. makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <-function(x = numeric())  {
      s <- NULL    
      set <- function(y) {    
            x <<- y    
            s <<- NULL    
      }
      get <- function() x    
      setsolve <- function(solve) s <<-solve    
      getsolve <- function() s    
      list( set = set, 
            get = get, 
            setsolve = setsolve, 
            getsolve = getsolve)    
}


## 2. cacheSolve computes the inverse of the matrix returned by makeCacheMatrix. 
## However if the inverse has already been calculated, stored and has not changed, then cacheSolve simply retrieves the inverse from the cache. 

cacheSolve <- function (x, ...) {
      s <- x$getsolve()    
      if(!is.null(s)) {    
            message("getting cached data")    
            return(s)    
      }
      data <- x$get()    
      s <- solve(data, ...)    
      x$setsolve(s)    
      s    ## Return a matrix that is the inverse of 'x'
}
