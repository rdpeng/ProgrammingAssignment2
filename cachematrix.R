## I'm really sorry, but I really have trouble with understanding, what I'm doing here.
## I've even had to ask Google, what is invertible matrix.
## I know, it's just a copy of example functions, but it seems to be working.

## I guess, that "m" is empty stock for future inverted matrix to be put to, "set" transfer x and m to Global environment 
## from the whole makeCacheMatrix, and "get" creates the very inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
       set <- function(y) {
           x <<- y
           m <<- NULL
       }
       get <- function() x
       setsolve <- function(solve) m <<- solve
       getsolve <- function() m
       list(set = set, get = get,
                       setsolve = setsolve,
                       getsolve = getsolve)
   }



## This function, if I'm getting it right, extracts result of "getsolve" from the global environment, if it was stored in "m" of 
## makeCacheMatrix, or just appies "solve", if it wasn't.

cacheSolve <- function(x, ...) {
     m <- x$getsolve()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
  }
