
## "makeCacheMatrix" will create four functions to set a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) { ## Creating the function "makeCacheMatrix"
  
  y <- NULL
  set <- function(z) { ## defines the value of the matrix
    x <<- z
    y <<- NULL
  }
  get <- function()    ## returns the value of the matrix
  setinverse <- function(solve) y <<- solve
  getinverse <- function() y
  list(set = set, get = get,
       set_inv = set_inv,  ## calculates the inverse of the matrix
       get_inv = get_inv)  ## gets the value of the inverse matrix
}


## "cacheSolve" checks if the function is cached and returns it

cacheSolve <- function(x, ...) {
    
    w <- x$get_inv()
    if(!is.null(w)) {    ## is there a inverse data in cache? So, return it
      message("getting cached data!")
      return(w)
    }
    data <- x$get() ## using x as a matrix, if the data is not in cache
    w <- solve(data, ...) ## calculates the inverse
    x$set_inv(w) ## putting data in cache
    w ## returning "w"
}
