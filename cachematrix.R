## Put comments here that give an overall description of what your
## functions do

## The 'makeCacheMatrix' function contains a list of 4 functions, namely set(), get(), setmatrix() and getmatrix()
## This function can be assigned to an object, e.g. a <- makeCacheMatrix(), and can be used to set and get a matrix or the inverse of its matrix
## The 'cacheSolve' function instead can be used to determine the inverse of a matrix or prints the already set out

## Write a short comment describing this function

## The set() function can be used to set a matrix, e.g. by a$makeCacheMatrix(x = matrix(c(2,1,5,3), nrow = 2,ncol = 2))
## The get() function prints out the matrix set before, e.g. by a$get()
## The Inverse of the matrix can be set by using the function setmatrix(), e.g. a$setmatrix(solve = matrix(c(3,-1,-5,2), nrow = 2,ncol = 2))
## The function getmatrix() prints the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
 m<-NULL
 set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## Write a short comment describing this function

## The 'cacheSolve' function is given an object containing the function 'makeCacheMatrix' an its List of 4 functions
## The 'cacheSolve' function prints out the inverse of a matrix. If it has been set before by using the 'makeCacheMatrix' function
## it simply prints out the set value. If the inverse of a matrix hasnt been determined, the 'cacheSolve' function calculates 
## the inverse of the matrix and prints it out.

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
