## makeCacheMatrix takes a matrix, and using it as an object
## creates getters and setters for it.
##
## cacheSolve takes a matix as an object, 
## and returns it's inveerse. If it already has
## calculated the inverse for this matrix and stores it in
## cached memory. If it gets another request for the same matrix,
## it'll note this and return the cached value.  If not, it'll
## calculate the inverse and store it in cached memory.

## An incoming matrix 'x' is set as an object and supplied with
## getters and setters.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse 
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## An object 'x' is tested to see if it already exists in cached memory.
## If it is, the cached value along with a note is returned.  If it is not
## cached, then the inverse of the matrix is caculated and cached. The inverse
## of the matrix is then returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  print(data)
  i <- solve(data, ...)
  x$setinverse(i)
}
