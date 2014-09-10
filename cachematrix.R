## Put comments here that give an overall description of what your
## functions do
## Coursera RPG07 course - jaimolto@gmail.com - Peer Assessments/Programming Assignment 2: Lexical Scoping

## Write a short comment describing this function
##Given a matrix "x", fucntion will create a metastructure of the matrix, as a list, with three elements, each of them a function
## 
## function get - Function to retrieve the original Matrix
## function setinv - Function to set up the inverse matrix in a global variable "cachedinv"
## function getinv - Fucntion to return the global variable "cachedinv" (cached inverse matrix) if available

## The metastructure will be used later on by function "cacheSolve" to minimize the effort to calculate inverse matrix
## by looking before calculate the inverse if the inverse has been already calculated abd is available as cachedinv

makeCacheMatrix <- function(x = matrix()) {

  cachedinv <- NULL
  get <- function() x
  setinv <- function(inv) cachedinv <<- inv
  getinv <- function() cachedinv
  list(get = get,
       setinv = setinv,
       getinv = getinv)

}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## we capture system start to be able to calculate proccessing time with and without cache
  start <- Sys.time()
  
  ## We first try to get the already cached inverse matrix calculated
  inv <- x$getinv()
  
  ## The first time the inverse of matrix x is calculated, it will need to be done using "solve" -- inv == NULL
  ## Subsequent calculations will just get the the value from x$getinv -- inv != NULL
  if(!is.null(inv)) {
    message("getting cached -solved inverse matrix- data")
    proccess_time <- Sys.time() - start
    message("Proccessing time - CACHED: ", proccess_time)
    return(inv)
  }
  
  ## Is the first time the inverse matrix is calculated, so we need to use solve
  OriMatrix <- x$get()
  inv <- solve(OriMatrix, ...)
  
  proccess_time <- Sys.time() - start
  message("Proccessing time - NON CACHED: ", proccess_time)
  
  ## Before returning the inv, we set up the cached value in cachedinv, so it can be retrieved on later 
  ## inverse calculation of the matrix using the function x$getinv
  x$setinv(inv)
  
  inv  
       
}


## Sample output

## > d=matrix(runif(10000),100,100)

## > dcache <- makeCacheMatrix(d)

## > dinv <- cachesolve(dcache)
## Proccessing time - NON CACHED: 0.00200009346008301

## > dinv <- cachesolve(dcache)
## getting cached -solved inverse matrix- data
## Proccessing time - CACHED: 0.00100088119506836

## > round(d %*% dinv)[1:10, 1:10]
      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
 [1,]    1    0    0    0    0    0    0    0    0     0
 [2,]    0    1    0    0    0    0    0    0    0     0
 [3,]    0    0    1    0    0    0    0    0    0     0
 [4,]    0    0    0    1    0    0    0    0    0     0
 [5,]    0    0    0    0    1    0    0    0    0     0
 [6,]    0    0    0    0    0    1    0    0    0     0
 [7,]    0    0    0    0    0    0    1    0    0     0
 [8,]    0    0    0    0    0    0    0    1    0     0
 [9,]    0    0    0    0    0    0    0    0    1     0
[10,]    0    0    0    0    0    0    0    0    0     1
