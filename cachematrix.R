## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
 set <- function(y) {
   x <<- y
   m <<- NULL
 }
 
 get <- function() x
 
 seti <- function(inv) m <<- inv
 geti <- function() m
 
 list( set = set, get = get,
       seti = seti , geti = geti)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$geti()
  if(!is.null(m)){
     message("Cache Inversed Matrix")
     return(m)
  }
  
  data <- x$get()
  m <- solve(data)
  x$seti(m)
  m
}
