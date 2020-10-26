## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## if the inverse has already been calculated (and the matrix has not chnaged, then cacheSolve should retrive the inverse from teh cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
