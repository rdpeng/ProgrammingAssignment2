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
     ## creates "set" function which returns "m"
     get <- function() x
     setmatrix <- function(solve) m <<- solve
     getmatrix <- function() m
     list(set=set, get=get,
          setmatrix=setmatrix,
          getmatrix=getmatrix)
}
## returns list of variables listed above

cacheSolve <- function(x=matrix(), ...) {
     ## creates "cacheSolve" function
     m <- x$getmatrix()
     if(!is.null(m)){
          message("getting cached data")
          return(m)
     }
     ## returns message above if function finds data for m (i.e., is not null)
     matrix <- x$get()
     m <- solve(matrix, ...)
     x$setmatrix(m)
     m
     ## finds inverse of matrix if it is not found to already be cached (e.g., not already solved)
}