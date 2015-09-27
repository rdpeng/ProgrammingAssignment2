## Put comments here that give an overall description of what your
## Using R's makeCacheMatrix, cache functionality a matrix can be inversed and cached (with goal of avoiding costly computations) using

## a floating function variable.The initial value referred to "m" is set as default Null value if CacheSolve has not been used (or doesn't contain a value)
## An enclosure environment (x<<- y) is set for the captured values of matrix to help keep count of the times the function is called.

makeCacheMatrix <- function(x = matrix()) {

}





makeCacheMatrix <- function(x =matrix()) {
  m <=NULL
  set <- function(y) {
  x <<-y
  m<<-NULL 
}

get<-function() x
setmatrix <- function(solve) m<<- solve
getmatrix <- function()m
list(set =set, get =get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}

## cacheSolve calls the "matrix" returned by MakeCacheMatrix above and computes its inverse. If inverse has been already 
## calculated it locates it value without calculating it again, and while searching itsd value(s) it shows a message 
## 'getting cache data'.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
cacheSolve <- function(x=matrix(), ...) {
   m < x$getmatrix()
   if (!is.null(m)) {
     message ("getting cached data")
     return (m)
     }

     matrix <- x$get
     m<- solve(matrix, ...)
     x$setmatrix(m)
     m
}