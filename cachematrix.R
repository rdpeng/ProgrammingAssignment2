## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

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

## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
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
################################################
##Test results

>makeCacheMatrix <- function(x = matrix()) {
+   m<-NULL
+   set<-function(y){
  +     x<<-y
  +     m<<-NULL
  +   }
+   get<-function() x
+   setmatrix<-function(solve) m<<- solve
+   getmatrix<-function() m
+   list(set=set, get=get,
         +        setmatrix=setmatrix,
         +        getmatrix=getmatrix)
+ }
> my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
> my_matrix$get()
[,1] [,2]
[1,]    1    3
[2,]    2    4
> my_matrix$getm()
NULL
> cacheSolve <- function(x=matrix(), ...) {
  +   m<-x$getmatrix()
  +   if(!is.null(m)){
    +     message("getting cached data")
    +     return(m)
    +   }
  +   matrix<-x$get()
  +   m<-solve(matrix, ...)
  +   x$setmatrix(m)
  +   m
  + }
> cacheSolve(my_matrix)
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> my_matrix$getm()
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5


 my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
 my_matrix$get()
[,1] [,2]
[1,]    2    1
[2,]    2    4
 my_matrix$getm()
NULL
 cacheSolve(my_matrix)
[,1]       [,2]
[1,]  0.6666667 -0.1666667
[2,] -0.3333333  0.3333333
> my_matrix$getm()
[,1]       [,2]
[1,]  0.6666667 -0.1666667
[2,] -0.3333333  0.3333333

