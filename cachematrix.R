## This is a pair of functions that can cache a matrix (wish is presume to be an invertible matrix)
## and also solves its inverse and cache it with its original matrix.

## The first function, `makeCacheMatrix` creates a special "matrix", which is
## really a list containing 4 functions:

## 1.  set the value of the matrix (and erase the cache inverse of a matrix if there was a previous matrix and its inverse)
## 2.  get the value of the matrix
## 3.  set the value of the inverse of the matrix
## 4.  get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv_mtrx <- NULL
    set <- function(y = matrix()) {
        x <<- y
        inv_mtrx <<- NULL
    }
    get <- function() x
    setinv <- function(inv) inv_mtrx <<- inv
    getinv <- function() inv_mtrx
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The second function `cacheSolve` computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache. Also,
## after the computation it will cache the solution un the special
## "matrix" so that it won't have to compute it anymore. 


cacheSolve <- function(x, ...) {
    inv_mtrx <- x$getinv()
    if(!is.null(inv_mtrx)) {
        return(inv_mtrx)
    }
    data <- x$get()
    inv_mtrx <- solve(data)
    x$setinv(inv_mtrx)
    inv_mtrx
}

