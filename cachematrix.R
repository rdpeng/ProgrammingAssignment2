## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
## Vogel-Peer-Review-Coursera-Week-3
## makeCacheMatrix <- function(x = matrix()){}
## makeCacheMatrix <- function(x = matrix()){ }
## Overall Description of project and function
## Purpose is to demonstrate the inverse function in the "makeCacheMatrix""
## This is a simple function in which you input the matrix and use the second to 
## caculate. And if the matrix remains the same, the second function will cache the result from memory instead of 
## caculating again.

## The first function creates a R object - list of functions that stores a matrix and its inverse. 
## When a new matrix is added, the inverse will be assigned as a NULL 
## The assignment should be made to the parent environment, so the value assigned to within the function is accessible after the function ends. 

# makeCacheMatrix <- function(x = matrix()){
i <- NULL
set <- function(y) {
        x <<- y
        i <<- NULL
        get <- function() x
        setinverse <- function(inverse)  i <<- inverse
        getinvere <- function() i
        list (set = set, get = get,
              setinverse = setinverse
              getinverse = getinverse)
}


## The second function first checks the inverse. If it's NULL then
## caculate and return the inverse. If it's not NULL then skip 
## caculation and retrun the inverse.
cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
        ## Return a matrix that is the inverse of 'x'
