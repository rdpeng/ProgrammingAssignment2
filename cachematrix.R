## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL 
        set <- function(y) { 
                x <<- y 
                inv <<- NULL 
        } 
        get <- function() x 
        setinverse <- function(inverse) inv <<- inverse 
        getinverse <- function() inv 
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 

}


## Write a short comment describing this function
## solve the inversed matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse() 
        if(!is.null(inv)) { 
                message("getting cached data.") 
                return(inv) 
        } 
        data <- x$get() 
        inv <- solve(data) 
        x$setinverse(inv) 
        inv 
}
