## This is the solution for programming assignment nr 2 in the Coursera R Programming Course
## written by Alex Dodoi

## This is the function that creates the special matrix and it is actually a list of functions

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse = matrix()) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The second funcion pritns the inverse if there is one assigned; if not it calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
