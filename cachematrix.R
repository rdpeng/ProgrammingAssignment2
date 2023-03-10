
#set the input x as matrix
#set the solved value "s" as null
#changed reference from "mean" to solve"
#makeCacheMatrix is a function which creates a special "matrix" object that can cache its inverse for the input
makeCacheMatrix <- function(x = matrix(1:50,9,3,3)) {

  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


#changed reference from "mean" to "solve" and "m" to "s"
#cacheSolve is a function which computes the inverse of the special "matrix" returned by makeCacheMatrix above
#If the inverse has already been calculated(and the matrix has not changed), then the cachesolve should retrieve theinverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting inversed matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
