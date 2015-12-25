## Put comments here that give an overall description of what your
## functions do
## Below two functions for matrix inversion in R.
## Their goal is to minimise the effort, by no repeating 
## the computation for the matrix whose inverse
## has already being generated in the session. 
## The first function “makeCacheMatrix” generates 
## a special “matrix’ object that can cache its inverse.
## the second function “cacheSolve“ computes the inverse of the special
## “matrix” returned by makeCacheMatrix. If the inverse has been calculated (and the matrix has
## not changed) then the cacheSolveMatrix should retrieve the inverse from the cache
## Write a short comment describing this function
## This function creates a special object which is a list that contains a function to
## set the value of the matrix
## get the value of the matrix
## set the inverse of the matrix
## get the value of the inverse
## NOTE: The matrix to be used should have an inverser 
## Its determinant must no be zero
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(inverse) m <<- inverse
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}
## Write a short comment describing this function
## This function calculates the inverse of the special "matrix"
## created with the previous function.
## First it checks whether the inverse has already been
## calculated. If so it gets the inverse from the cache, and
## skips the calculation. Otherwise it calculates the inverse
## of the matrix and sets the value of the inverse in the cache.
## using the “setmatrix” function
## get the value of the matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}