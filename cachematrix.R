makeCacheMatrix <- function(mat = matrix()) {
    # initializing the inverse to be NULL
    inv <- NULL
    
    # the setter for the matrix
    set <- function(mtrx) {
        mat <<- mtrx
        inv <<- NULL
    }
    
    # the getter for the matrix
    get <- function() mat
    
    # the setter of the inverse
    set.inverse <- function(setinv) inv <<- setinv

makeCacheMatrix <- function(mat = matrix()) {
    # initializing the inverse to be null
    inv <- NULL
    
    # the setter for the matrix
    set <- function(mtrx) {
        mat <<- mtrx
        inv <<- NULL
    }
    
    # the getter for the matrix
    get <- function() mat
    
    # the setter for the matrix
    set.inverse <- function(setinv) inv <<- setinv
## created the matrix with a getter and a setter

makeCacheMatrix <- function(mat = matrix()) {
    # initializing the inverse to be null
    inv <- NULL
    
    # the setter for the matrix
    set <- function(mtrx) {
        mat <<- mtrx
        inv <<- NULL
    }
    
    # the getter for the matrix
    get <- function() mat
    
    # the setter for inverse
    set.inverse <- function(setinv) inv <<- setinv

makeCacheMatrix <- function(mat = matrix()) {
    # initializing the inverse to be null
    inv <- NULL
    
    # the setter for the matrix
    set <- function(mtrx) {
        mat <<- mtrx
        inv <<- NULL
    }
    
    # the getter for the matrix
    get <- function() mat
    
    # the setter for inverse
    set.inverse <- function(setinv) inv <<- setinv

    # the getter for inverse
    get.inverse <- function() inv

    # the list of the setter and getter functions
    list(set = set, get = get,
         set.inverse = set.inverse,
         get.inverse = get.inverse)
}
## the cache solved the inverse of the matrix
## and if it is already calculated the returns cached
## you may use results instead
cacheSolve <- function(cached.mat, ...) {
    # gets the value of the stored inverse
    inv <- cached.mat$get.inverse()
    # if it exists then return it
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
   
}
## the cache solved the inverse of the matrix
## and if it is already calculated the returns cached
## you may use results instead
cacheSolve <- function(cached.mat, ...) {
    # gets the value of the stored inverse
    inv <- cached.mat$get.inverse()
    # if it exists then return it
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
  
}
## the cache solved the inverse of the matrix
## and if it is already calculated the returns cached
## you may use results instead
cacheSolve <- function(cached.mat, ...) {
    # gets the value of the stored inverse
    inv <- cached.mat$get.inverse()
    # if it exists then return it
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
        
    raw.mat <- cached.mat$get()
    inv <- solve(raw.mat, ...)
    cached.mat$set.inverse(inv)

    # the it returns the inverse
    inv
}
