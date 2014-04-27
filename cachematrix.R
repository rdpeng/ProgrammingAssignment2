##The following functions return a special matrix with the ability to cache the inverted matrices
##should one need to call the function repeatedly.

#This function creates special matrix object 'matrix' which contains a function to cache its inverse
makeCacheMatrix <- function(x = matrix()){
    m <- NULL
    set <- function(y){
        x<<-y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- m
    getinverse <- function() m
    list(set=set, get=get,
         setinverse=setinverse, 
         getinverse=getinverse)
}

#This function returns the inverse of the 'matrix' object unless it's already cached and remains unchanged
cacheSolve <- function(x, ...){
    #Returns cache (if it exists)
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    #If cache doesn't exist then solve for and return inverted matrix
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}