## The following functions return a special matrix with the ability
## to cache inverted matrices should on need to call the function repeatedly.

## This funciton created a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<-NULL
}
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
        }

## This function returns the inverse of the matrix object

cacheSolve <- function(x=matrix, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)){
                message"getting cached data"
                return(inv)
}
        matrix <- x$get()
        inv <- solve(matrix)
        x$getinverse(inv
                     inv 
}
