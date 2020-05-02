## Below are two functions that are used to create a special object that
## stores a matrix and cache's its inverse.

## The first function, makeCacheMatrix creates a special "matrix" object
## in a list containing a function to
## 1.set the value of the input matrix in cache
## 2.get the value of the cache matrix
## 3.set the value of the inverse of the matrix
## 4.get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get <- function() x
  setinv <- function(inv) m<<-inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve function takes the function created above as an input and
## returns the inverse of matrix stored in cache via makeCacheMatrix
## function.
## If the inverse of the matrix stored in cache is already calculated, it
## skips the computation part and pulls the inverse from the cache.

cacheSolve <- function(x, ...) {
  m<-x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m <- solve(data,...)
  x$setinv(m)
  m
}

## Below is an example to illustrate the above two functions

mat<-matrix(c(1:8,8),3,3,byrow = T)
k<-makeCacheMatrix(mat)
k$get()
k$getinv()
cacheSolve(k)
k$get()
k$getinv()
cacheSolve(k)
