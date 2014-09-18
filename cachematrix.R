## Put comments here that give an overall description of what your
## functions do
## These functions will cache the inverse of the matrix after the first calculation, 
## rather than compute it everytime when i need it.

## Write a short comment describing this function
## This function creates a matrix which can cached and inverse

makeCacheMatrix <- function(x = matrix()) {
 inv<-NULL
## setting the matrix and 'inv' to NULL
 set<-function(y){
         x<<-y
 inv<<-NULL}
##get returns the matrix
 get<-function()x
## set the inverse of the matrix
 setinv<-function(x_inv) inv<<-x_inv
## get the inverse of the matrix
 getinv<-function()inv
##setting the namea to the functions
 list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function
## This function can calculate the inverse of the matrix returned by makeCacheMatrix
## Also, If the inverse of the matrix is already calculated
##      then it should be retrieved but it is not calculated, it will calculate the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inv<-x$getinv()
## if the inverse is alreay calculated, return it
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
## if the inverse of the matris didn't exit, get the matrix from above function
        data<-x$get()
## calculate the inverse of the matrix
        inv<-solve(data,...)
## caching the inverse for later
        x$setinv(inv)
## print calculated inverse of the matrix
        inv
}
