## Assignment:  Caching the inverse of a Matrix
## Matrix inversion is a costly computation.
## Repeated computation during the calcuation of matrix inversion is really a time consuming activity.
## To Write a pair of functions which cache the inverse of a matrix.

## The first function "makeCacheMatrix", which is comprised of a list:
##(a) set the value of the matrix
##(b) get the value of the matrix
##(c) set the value of the matrix
##(d) get the value of the matrix

makeCacheMatrix <-function( x = matrix()){
  nra <- NULL
  set <- function(y) {
    x <<- y
    nra <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) nra <<-solve
  getinverse <- function() nra
  list(set = set , get = get ,
       setinverse = setinverse ,
       getinverse = getinverse )
}


## The second function "cacheSolve" computes the inverse of the special matrix returned by "makeCacheMatrix".
## It first check whether the inverse has been calculated,if so, then it gets the value from cache and skip computation.
## Or else , it calculates the inverse , set the value in the cache via setinverse function.
## Assume that matrix supplied is always invertible.

cacheSolve <-function(x, ...)  {
  nra <- x$getinverse()
  if(!is.null(nra)) {
    message("getting cached data")
    return(nra)
  }
  data <- x$get()
  nra <- solve( data, ...)
  x$setinverse(nra)
  nra
}
##solution:-
##nra <-matrix(c(5,6,7,1,3,4,1,9,4),3,3)
##> nra
##         [,1] [,2] [,3]
##   [1,]    5    1    1
##   [2,]    6    3    9
##   [3,]    7    4    4
## nra1<-makeCacheMatrix(nra)
## > cacheSolve(nra1)
##         [,1]       [,2]        [,3]
##  [1,]  0.30769231  0.0000000 -0.07692308
##  [2,] -0.50000000 -0.1666667  0.50000000
##  [3,] -0.03846154  0.1666667 -0.11538462
