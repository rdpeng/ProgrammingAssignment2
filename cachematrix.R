## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
## creates function with name "makeCacheMatrix"     

     m <- NULL
     ## creates empty variable named "m"
     set <- function(y){
          x <<- y
          m <<- NULL
     }
     ## creates "set" function which 
     get <- function() x
     setmatrix <- function(solve) m<<- solve
     getmatrix <- function() m
     list(set=set, get=get,
          setmatrix=setmatrix,
          getmatrix=getmatrix)
}

cacheSolve <- function(x=matrix(), ...) {
     m <- x$getmatrix()
     if(!is.null(m)){
          message("getting cached data")
          return(m)
     }
     matrix <- x$get()
     m <- solve(matrix, ...)
     x$setmatrix(m)
     m
}