## Put comments here that give an overall description of what your
## functions do
#These two functions cache the inverse of a matrix
#Please put inversable matrix in the function

## Write a short comment describing this function
#This function creats a special "matrix" and can cache its inversion. 

makeCacheMatrix <- function(x = matrix()) {
        m<- NULL
        set<- function(y){
                x<<- y
                m<<- NULL
        }
        get<- function()x
        setinverse<- function(solve) m<<- solve
        getinverse<- function()m
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function
#This function computes the special "matrix" above. 
#Please assign the result from makeCacheMatrix() to a new object, put the new object into cacheSolve()
#If the inversion has been calculated, then this function return the inverse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m) 
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}