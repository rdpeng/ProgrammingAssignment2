## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

}

mi <- NULL 
mset <- function( matrix ){
        m <<- matrix 
        mi <<- NULL} # set the matrix 
mget <- function(){
        m} # returns the matrix
setinverse <- function(inverse){
        mi <<- inverse} # set the inverse of the matrix 
getinverse <- function(){
        mi} # returns thee inverse 

## Write a short comment describing this function


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse
        return(m)
data <- x$mget() # get the matrix from the object 
m <- solve(data)%*% data # calculate the inverse by multiplication
x$setinverse(m) 
m # returns the matrix 
}       




