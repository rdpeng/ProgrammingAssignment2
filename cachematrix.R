## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()){
     m <- NULL
     set <- function(y){
         x <<- y # <<- only for function use
         m <<- NULL
     }
     get <- function()x #call the function
     setinverse <- function(inverse) m <<-inverse 
     #set the inverse function
     getinverse <- function() m
     #get the inverse function
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
     #create a list to store the set,get method
 }
## Write a short comment describing this function
cacheSolve <- function(x,...){
     ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse() # call the get function
         if(!is.null(m)){
             # check the null value
             message("getting cached data")
             return(m)
         }
     data<- x$get()
     m <- solve(data,...)
     x$setinverse(m)
     m
 }