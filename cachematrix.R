## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL    # i will be our inverse and will reset to NULL each time the makeCacheMatrix is called
  
  # the next 3 functions are defined but not called when makeCacheMatrix is called
  # the functions will be used by cachesolve() to get values for x and for setting the mean.
  # such objects are methods
  
  get <- function() {x} # this function returns value of matrix
  
  setinv <- function(solve) {inv <<- solve}
  # this will be called by cacheSolve() for first cachSolve() and will store it using an assignment
  
  getinv <- function() {inv}  #the returns cached value to cacheSolve() whenever accessing
  
  list(get = get, 
       setinv = setinv,
       getinv = getinv)
  #This gets accessed each time makeCacheMatrix is called, and each time a new object is created. This is a list of functions -"methods" as described above- so the calling function knows how to access these "methods
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) { # the input x is the inverse (hopefully) of makeCacheMatrix
  inv <- x$getinv()    # accesses the object x and gets the inverse
  if(!is.null(inv)) {   # if inverse was already cached 
    
    message("getting cached data")     # sends this message to the console
    return(inv)                          # returns the inverse and, subsequently, ends the function cacheSolve()
  }
  data <- x$get()    # we get this code if x$getinv() returns NULL
  inv <- solve(data, .. ) # if inv was NULL then inverse is computed
  x$setinv(inv)           # stores the computed inverse values in x (see function above)
  
  
{
    
  }
## Return a matrix that is the inverse of 'x'
}
