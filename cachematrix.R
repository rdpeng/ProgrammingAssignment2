
makeCacheMatrix <- function(x = matrix()) {
     
     inv <- NULL
     set <- function(y) {
          ## set is a function that changes the vector stored in the main function
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     ## get is a function that returns the vector x stored in the main function
     
     setinverse <- function(inverse) inv <<- inverse
     getinverse <- function() inv
     ## setinverse and getinverse only stores the value of inverne in a variable 
     ## "inv" into the main function
     
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
     
}

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     
     inv <- x$getinverse()
     if(!is.null(inv)) {
          ## if the inverse has already been calculated...
          message("getting cached data")
          return(inv)
     }
     ## else, calculate the inverse of x by the (native) function "solve"
     data <- x$get()
     inv <- solve(data, ...)
     x$setinverse(inv)
     inv
}
