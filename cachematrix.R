###########
# coursera R
# Peer Assessments /Programming Assignment 2: Lexical Scoping 
# Hard Deadline   Sun 22 Jun 2014 4:30 PM PDT
# Sorry. I cannot finish my assignment. I just wrote what I think but it dose not work yet.
##########
rm (list=ls())



## four sub-functions to make inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    get <- function() x

    #I do not know to make it work...
    setsolve <- function() {
      x <<- solve(x) %*% x 
    }
    
    
    getsolve <- function() m
    
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
  }



## if it is cached, do not calculate it again. 
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




#### let's run it.
a= makeCacheMatrix( matrix( 1:4, 2, 2) )
cachSolve(a)
# return inverse matrix

cachSolve(a)
# should return cached inverse matrix
