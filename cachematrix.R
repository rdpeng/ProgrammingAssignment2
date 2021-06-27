##Function 1: makeCacheMatrix(x)
##This function plays four critical roles (listed in sequential order)
##Sets the value of the matrix using the double arrows to modify parent levels
##Gets the value of the matrix
##Sets the value of the inverse (uses double arrows again)
##Gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      inver <- NULL
      set <- function(y) { 
            x <<- y
            inver <<- NULL
            }
      get <- function() {
            x  } 
      setinv <- function(inverse){
            inver <<- inverse }
      getinv <- function() {
            inver }
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}

##Function 2: cacheSolve(x,...)
##This function will be evaluate the inverse of the matrix dvlped prvsly
##It first checks to see if the "inver" is NULL
##    -if it is not, it will retrieve the inverse from the cache
##    -if it is, it will invert the matrix and "setinv"


cacheSolve <- function(x, ...) {
        inver <- x$getinv()
        if(!is.null(inver)) { 
              message("getting cached data")
              return(inver) }
        results <- x$get()
        inver <- solve(results, ...)
        x$setinv(inver)
        inver
}
