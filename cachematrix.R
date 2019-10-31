## Put comments here that give an overall description of what your
## functions do

## My solution

makeCacheMatrix <- function(x = matrix()) { ### Creating makeCacheMatrix function
  In <- NULL
  set <- function(y) {
    x <<- y
    In <<- NULL
  }
  get <- function() x
  set_inv <- function(solve) In <<- solve
  get_inv <- function() In
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) { ### Creating cacheSolve function
  In <- x$get_inv()
  if(!is.null(In)) {
    message("getting cached data")
    return(In)
  }
  data <- x$get()
  In <- solve(data, ...) ### Using Solve Function
  x$set_inv(In)
  In
}
