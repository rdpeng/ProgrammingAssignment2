## makeCacheMatrix stores a matrix of results and returns it on request.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL #Setting inv to null
    set <- function(y) { # sets up the matrix
      x <<- y
      inv <<- NULL
    }
    get <- function() x #gets the matrix
    setinv <- function(inverse) {inv <<- inverse} #sets the inverse of the matrix
    getinv <- function() inv #gets the inverse
    list(set = set, get = get, #makes a list of arguments
         setinv = setinv,
         getinv = getinv)

}


# cacheSolve either pulls solution from makeCacheMatrix or generates an inverse of the matrix.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()

# checking to see if the inverse is cached.
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
#get the matrix
    data <- x$get()
#compute the inverse
    inv <- solve(data)
#caches the inverse
    x$setinv(inv)
#returns the inverse
    inv
  }
