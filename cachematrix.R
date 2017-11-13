## There are two functions here:  makeCacheMatrix and cacheSolve.
## The input to makeCacheMatrix is a matrix, the output is a list of functions
## The input into cacheSolve is the output of makeCacheMatrix
## The two functions work together to return the inverse of a square matrix using the cacheSolve function.

## If you want to get the inverse of a square matrix, you feed the matrix to makeCacheMatrix
## Then you call cacheSolve on the output of makeCacheMatrix

## To test this i created a square matrix m
## To get the inverse of m you can call (type at command line): cacheSolve(makeCacheMatrix(m)) 
## or You can first assign the output of makeCacheMatrix(m) to an object (e.g. a <- makeCacheMatrix(m)), then call: cacheSolve(a)


## The input to makeCacheMatrix  is a matrix x.  
## First, a null object i is initialized 
## Then 4 functions are defined (set, get, setinverse, getinverse)
## The output is a list that contains and names the functions defined in makeCacheMatrix.
## Naming them makes them easy to call them in cacheSolve

makeCacheMatrix <- function(x = matrix(numeric())){
  i <- NULL
  set <- function(y){
    x <<- y
    i <- NULL
  }
  get <- function() x
  setinverse <- function(source) i <<- source
  getinverse <- function() i
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## The function cacheSolve takes the output from makeCacheMatrix (a list of functions)
## First it gets whatever is stored in cache from makeCacheMatrix by calling x$getinverse() and assigning to i
## it checks to see whether i is null; if it's not null then it returns i (whatever inverse matrix was stored in cache)
## if i is null (nothing stored in cache) then it gets the input matrix by calling x$get(), assigns it to an object called data
## then it takes the inverse of the data matrix using solve(data,...) and assigns it to i
## then it returns the inverse matrix i


cacheSolve <- function(x, ...){
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}