## Assignment 2  - George Brenckle
## The assignemnt is to write a pair of functions to cache the inversion of a matrix
## -- the first creates a special matrix object that can cache the inverse
## -- the second either computes the inverse (if it has not been cached) or returns the cached inverse from
## -- the special matrix object

## This is modeled after the makeVector and cachemen functions provided in the instructions for assignment 2
##

## MakeCacheMatrix created a list containing four functions: 1, setting the matrix, 2. retrieving the matrix
## -- 3. setting the inverse of the matrix, and 4 retrieving the inverse.

makeCacheMatrix <- function (x = matrix()) {
    inv <- NULL
    set <- function(y){
        x  <<- y
        inv <<- NULL
    }
    get <- function () x
    setinv <- function (solve) inv <<- solve(x)
    getinv <- function () inv
    list (set = set, get = get,
    setinv = setinv,
    

## This function calculates the inverse of the matrix using the solve function.  First it checks to see if
## -- this has already been accomplished and the inverse cached.  If so, it returns the cache.  If not, it
## -- calculates the in verse, caches it, and returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    inv
    if (!is.null(inv)) {
        message("getting cached data")
        return (inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv (inv)
    inv
}

## the following code tests the functions for a 2x2, 3x3, 4x4, and 5x5 matrix.  The matrices were selected
## -- such that they did not have a determinant of 0 and therefore had a valid inverse.  These functions
## -- will not work on a matrix that does not have a valid inverse.

## aa is my special object matrix created by using the makeCacheMatrix function

aa <- makeCacheMatrix()
aa

## m2, m3, m4, m5 are my test matrices (2x2, 3x3, 4x4, and 5x5

m2 <- matrix (c(0,2,1,0), nrow=2, ncol=2)
det(m2)
m3 <- matrix (c(0,1,2,3,0,4,5,1,0), nrow=3, ncol=3)
det(m3)
m4 <- matrix (c(1,2,3,2,1,1,4,3,3,2,3,1,1,4,1,5), nrow=4, ncol=4)
det(m4)
m5 <- matrix (c(1,2,1,3,1,4,1,5,2,2,2,3,2,4,2,5,3,1,3,2,3,3,4,3,1), nrow=5, ncol=5)
det(m5)


## test with the 2x2 matrix.  Validate by multiplying the original with the inverse and showing that it is
## the identify matrix

aa$set(m2)
aa$get()
aa$getinv()
cacheSolve(aa)
cacheSolve(aa)
round(aa$getinv() %*% aa$get())
round( aa$getinv() %*% aa$get()) == diag(nrow = nrow(m2), ncol = ncol(m2))

## repeat for 3x3

aa$set(m3)
aa$get()
aa$getinv()
cacheSolve(aa)
cacheSolve(aa)
round(aa$getinv() %*% aa$get())
round(aa$getinv() %*% aa$get() )== diag(nrow = nrow(m3), ncol = ncol(m3))

## repeat for 4x4

aa$set(m4)
aa$get()
aa$getinv()
cacheSolve(aa)
cacheSolve(aa)
round( aa$getinv() %*% aa$get())
round( aa$getinv() %*% aa$get()) == diag(nrow = nrow(m4), ncol = ncol(m4))

## repeat for 5x5

aa$set(m5)
aa$get()
aa$getinv()
cacheSolve(aa)
cacheSolve(aa)
round (aa$getinv() %*% aa$get())
round ( aa$getinv() %*% aa$get()) == diag(nrow = nrow(m5), ncol = ncol(m5))

