## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##This function setsthe value of the inverse matix

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set <- function(y){
                x<<- y
                i<<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set=set, get=get,
             setinverse=setinverse,getinverse=getinverse)
}





## Write a short comment describing this function

## this function looks for the inverse in the cache data and returns it
## if available if not it caculates the inverse 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cashed data")
                return (i)
        }
        data <- x$get()
        i <- solve (data, ...)
        x$setinverse(i)
        i
}
