
Skip to content
This repository

    Pull requests
    Issues
    Marketplace
    Explore

    @CaoTungPHAM

0
0

    0

CaoTungPHAM/R_Assignment02
Code
Issues 0
Pull requests 0
Projects 0
Wiki
Insights
Settings
R_Assignment02/cachematrix.R
450f8a5 on 24 Oct 2017
@CaoTungPHAM CaoTungPHAM Add files via upload
62 lines (50 sloc) 1.95 KB
## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to: 

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse

## For example, if X is a matrix, and if makeCacheMatrix(X) is denoted 
## by Y, then: 

## 2. Y$get() returns the matrix X.
## 1. If Z is another matrix, then Y$set(Z) transforms the cache matrix 
##    into Z. Consequently, Y$get() now gives Z. 
## 4. Y$getInverse() gives NULL, because we have not calculated 
##    it's inverse.
## 3. If Z is a matrix, Y$setInverse(Z) sets Z to be the cache inverse 
##    matrix. Consequently, makeCacheMatrix(Y) should give Z as response,  
##    with the message "getting cached data".


makeCacheMatrix <- function(X = matrix()) {
  Inv<- NULL
  set <- function(Y) {
    X <<- Y
    Inv <<- NULL
  }
  get <- function() X
  setInverse <- function(Inverse) Inv <<- Inverse
  getInverse <- function() Inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The following second function calculate the inverse of the 
## cache "matrix" returned by makeCacheMatrix above, which can be 
## denoted by Y for example. There are 3 cases:

## 1. If the inverse matrix has been calculated then it gets the 
## inverse from the cache, mentioning "getting cached data".

## 2. If the inverse matrix has been assigned to be a matrix Z, 
## by "Y$setInverse(Z)", then the cached inverse matrix is simply 
## Z, with the message "getting cached data".

## 3. If the inverse matrix has not been calculated or assigned 
## to be anything, then it calculates the inverse.

cacheSolve <- function(X, ...) {
  Inv<- X$getInverse()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  data <- X$get()
  Inverse <- solve(data, ...)
  X$setInverse(Inverse)
  Inverse
}

    © 2018 GitHub, Inc.
    Terms
    Privacy
    Security
    Status
    Help

    Contact GitHub
    API
    Training
    Shop
    Blog
    About
