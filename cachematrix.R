## Author - Sharan
## Written on 16th June, 2018
## Purporse - Coursera Assignment 2

## There are two functions - makeCacheMatrix & cacheSolve. 
## 'makeCacheMatrix' creates a special matrix, that contains the set of 
## functions to store a matrix, fetch a matrix if stored, compute the Inverse 
## of the matrix assuming that the matrix is invertible, and fetch the Inverse 
## of the matrix if it exists!
## 'cacheSolve' computes the inverse of the matrix. If the inverse already 
## exists for that matrix, then it just gets the stored inverse, thus saving 
## computational time

## 'makeCacheMatrix' function takes a matrix as an input and creates a list of
## four functions - set, get, setinverse, getinverse for that matrix. 
## Set functions (both set & setinverse) lets you set the value of the input 
## matrix and its computed inverse
## Get functions (both get & getinverse) lets you fetch the value of the input 
## matrix and its computed inverse if it is already stored for the same input
## matrix in the functional environment

makeCacheMatrix <- function(x = matrix()) {
    matr <- NULL
    set <- function(y) {
        x <<- y
        matr <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) matr <<- solve
    getinverse <- function() matr
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## 'cacheSolve' function lets you compute the inverse of an matrix. If the 
## inverse of that matrix already exists in memory, then it fetches the inverse
## thus saving computational time

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    matr <- x$getinverse()
    if(!is.null(matr)) {
        message("getting cached data")
        return(matr)
    }
    data <- x$get()
    matr <- solve(data, ...)
    x$setinverse(matr)
    matr
}
