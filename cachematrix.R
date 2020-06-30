## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## Function creates a special "matrix" object that can cache its inverse
        i <- NULL
        set <- function(y) {
          x <<- y
          i <<- NULL
        }
        get <- function() x
        set_solve <- function(solve) i <<- solve
        get_solve <- function() i
        list(set = set, get = get,
             set_solve = set_solve,
             get_solve = get_solve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$get_solve()
        if (!is.na(i)){
          message("getting cached data")
          return(i)
        }
        data <- x$get()
        i <- solve(x)
        x$set_solve(i)
        i
}
