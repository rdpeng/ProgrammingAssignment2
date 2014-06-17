## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

  store <- NULL
  set <- function(y) {
    x <<- y
    store <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) store <<- solve
  getmatrix <- function() store
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

}


## Write a short comment describing this function
##cacheSolve: This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
         store <- x$getsolve()
  if(!is.null(store)) {
    message("getting cached data")
    return(s)
  }
  getdata <- x$get()
  store <- solve(data, ...)
  x$setsolve(store)
  store
}
        

