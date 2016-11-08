
#this function sets the value of matrix, gets the value of matrix, sets the inverse of matrix, gets the inverse of matrix 


 makeMatrix <- function(x = matrix()) {
     MATINVE <- NULL    #takes null value in global environment
     set <- function(y) {
         x <<- y
         MATINVE <<- NULL  # null value in local environment
     }
     get <- function() x
     setinverse <- function(inverse) MATINVE <<- inverse
     getinverse <- function() MATINVE 
     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
 }
 
 
 
 
 
 cachematrix <- function(x, ...) {
     MATINVE <- x$getinverse()       #takes cache data from makeMatrix function if any
     if(!is.null(MATINVE )) {
         message("getting cached matrix data.")     #if cached data is pesent it is copied and executed
         return(MATINVE )
     }
     data <- x$get()                 #if no cached data is present from previous function inverse of matrix is solved
     MATINVE<- solve(data)
     x$setinverse(MATINVE)
     MATINVE 
 }



