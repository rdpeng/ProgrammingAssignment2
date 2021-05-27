#function to cache the inverse of a matrix
makeCacheMatrix <- function(x = matrix() ) {
          m <- NULL
          
          #set the value of the matrix
          set <- function(y) {
                    x <<- y
                    m <<- NULL
          }
          
          #get the value of the matrix
          get <- function() x
          
          #set the value of the inverse
          setsolve <- function(solve) m <<- solve
          
          #get the value of the inverse
          getsolve <- function() m
          list(set = set, get = get,
               setsolve = setsolve,
               getsolve = getsolve)
}

#function to calculate the inverse of the matrix as specified above
cacheSolve <- function(x, ...) {
          m <- x$getsolve()
          
          #checks if the inverse has already been calculated. 
          #if so, it pulls solution from cache.
          if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
          }
          data <- x$get()
          
          #calculation of inverse matrix
          m <- solve(data, ...)
          x$setsolve(m)
          m
}
# mvec$setsolve(x)
# mvec$getsolve()
# 
# #example of how cachemean works
# cacheSolve(mvec)
