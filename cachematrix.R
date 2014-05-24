## Put comments here that give an overall description of what your
## functions do
if(FALSE){
  makeCacheMatrix is a function that takes a matrix as an argument,
  and keeps its inverse in a function-held list to be searched for later use by 
  the other function cacheSolve - which again takes the cached matrx as an argument
  which finally returns the required and stored existing inverse ;
  otherwise it caluclates it and inserts it in the cached list.
  It is assumed that the matrix declared is always invertible.
}

## Write a short comment describing this function
if(FALSE){
  The function makeCacheMatrix returns a list containing various other functions 
  as it's elements.' All elements of the list are generated from within it.
  - get is a variable assignment of the argument x; can be used on capture by
  cachSolve function
  - setinv is the variable-declared inverse solution; can be used on capture by
  cachSolve function
  - getinv is a variable assignment of the function solve; can be used on capture by
  cachSolve function.
}

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinv <- function(solve) s <<- solve
  getinv <- function() s
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Write a short comment describing this function
if(FALSE){
  s is a variable assigned the element getinv, whatever it might find.
  the if-statement checks if something already exists in the list for the given cached matrix argument
  and returns the solution
  otherwise:
    a) it gets the matrix via `get` to prepare it for caching and assigns the variable data to it;
    b) gets the inverse of the said matrix;
    c) inserts it in the list capturing the `setinv` function within makeCacheMatrix and returns
      the solution it just calculated.
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getinv()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinv(s)
  s
}
