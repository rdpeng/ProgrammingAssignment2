## In fact functions nead only to change computation function from mean to solve, rest works at the same way, with setters and getters. 
## Small difference is also an function argument, that is not a vector, but matrix, as per assignement, there is not need to check whenever
## matris is invertable, as we assume it is. 

makeCacheMatrix <- function(x = matrix()) { # first function take matrix argument, we do not check whenever matris is invertible
  m <- NULL                                 # declare value
  set <- function(y) {                      # setter function definition
    x <<- y
    m <<- NULL      
  }
  get <- function() x                        # getter function definition
  setinverse <- function(whatever) m <<- whatever
  getinverse <- function() m
  list(set = set, get = get,                 # returm list with named values, to allow acces by $
       setinverse = setinverse,
       getinverse = getinverse)
}


## This is function, that calculates inverse (solve) if not yet inverted erlier. 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()                     
  if(!is.null(m)) {                         # check if matrix needs to be inversed, or value can be taken from memory
    message("getting cached data of inveresed matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)                     # real inverse matrix computation is done in this place
  x$setinverse(m)
  m
} 
