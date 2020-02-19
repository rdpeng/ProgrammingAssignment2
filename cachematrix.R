## These two functions are used to compute the inverse of a matrix 
## OR retrieve the stored inverse of a matrix depending on whether 
## it has been previously computed.

## This function returns a list of functions which are used to
## store/retrieve x and it's inverse.
## The list create b this function is the form of the argument 
## required for the "cacheSolve" function below.

makeCacheMatrix <- function(x = matrix()) {
      mi <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(minv) mi <<- minv
      getinv <- function() mi
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}

## This function returns the inverse of x as defined in makeCacheMatrix().
## The function will compute the inverse of x using solve() but after
## checking that there is not inverse previously stored.
## NOTE: the function will retun an error if x has no inverse.

cacheSolve <- function(x, ...) {
      mi <- x$getinv()
      if(!is.null(mi)) {
            message("getting cached data")
            return(mi)
      }
      data <- x$get()
      mi <-solve(data,diag(nrow=nrow(data)),...)
      x$setinv(mi)
      mi
        ## Return a matrix that is the inverse of 'x'
}
