# Define the class makeCacheMatrix()
makeCacheMatrix <- function(x = matrix()) {
# Properties section
  im <- NULL # this property is for storing the inverse matrix for "x"
  # "x" is also a property for this class
# Methods section
  set <- function(y) {
    if(!is.matrix(y) | is.null(y)) y <- matrix()
    x <<- y
    im <<- NULL
  }
  get     <- function() x
  setInvMtrx <- function(InvMtrx) {
    if(!is.matrix(InvMtrx) | is.null(InvMtrx)) InvMtrx <- NULL
    im <<- InvMtrx
  }
  getInvMtrx <- function() im
# Initialize section
  if(!is.matrix(x) | is.null(x)) x <- matrix()
# Publish section
  list(set = set, 
       get = get,
       setInvMtrx = setInvMtrx,
       getInvMtrx = getInvMtrx)

}

# This is a function not a class
cacheSolve <- function(x, ...) { # x is an object of makeVector() class
  m <- x$getInvMtrx()

  if(!is.null(m)) {
    message("getting cached data")
    return(m) # function terminates here
  }

  out <- tryCatch( # catch exceptions
    {
      mtx <- x$get()
      m   <- solve(mtx, ...) # compute the inverse matrix of mtx
    },
    error = function(e) {
      message(e)
      return(NULL)
    },
    warning = function(e) {
      message(e)
      return(NULL)
    }
  )
  x$setInvMtrx(out)
  return(out)
}
