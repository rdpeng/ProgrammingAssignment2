## Put comments here that give an overall description of what your
## functions do
## 
## The functionality of the functions is the following. The 2 functions combined together yield the inverse of a matrix and cache the inverse on the memory. So, when the input data have not changed, then the second function "reads" the inverse of the matrix, rather than recomputing it. Otherwise, the inverse of the matrix is computed from the beginning.

## Write a short comment describing this function

##The function 'makeCacheMatrix' takes as input a matrix and outputs 4 values, namely 'set','get','getinv', 'setinv'. The functions 'set' and 'setinv' are the "setters" and the functions 'get' and 'getinv' are the "getters". The 'set' function 'sets' the value of the matrix and the 'setinv' function 'sets' the value of the inverse. The functions 'get' and 'getinv' retrieve the matrix and the inverse of the matrix, respectively.
##

makeCacheMatrix <- function(x = matrix()) {
    inv<- NULL
    
    set <- function(y){
        x<<- y
        inv<- NULL
    }
    
    get<- function() x
    setinv<- function(r) inv<<- r
    getinv<- function() inv
    
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
    
}



## Write a short comment describing this function

## The cacheSolve matrix retrieves the inverse matrix through the function 'getinv' (as defined above) and and assigns it to the variable 'inverse'. If the "inverse" variable is not null, then it retrieves the inverse of the matrix, without re-computing it. 
## 
## In the opposite case, the new data (matrix) are read through the function "x$get$" and the function "solve" computes the inverse of the matrix storing it in the variable "inverse".

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse<-x$getinv()
    if(!is.null(inverse)){
        message("getting the cached data")
        return(inverse)
    }
    data<- x$get()
    inverse<- solve(data, ...)
    x$setinv(inverse)
    inverse
    
}
