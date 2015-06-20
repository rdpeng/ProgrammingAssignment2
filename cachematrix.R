## my function "makeCacheMatrix" make a special matrix that can cache
## its inverse and my cacheinverse will computes the inverse returned
## by makecachematrix function. If inverse already exist, cacheSolve will
## retrieve the inverse from cache

## makeCacheMatrix will creative a matrix that can cache it's inverse


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {  #setting matrix
                x <<- y 
                m <<- NULL
        }
        get <- function() x  #retreiving matrix or inverse
        setinverse <- function(solve) m <<- solve #setting inverse
        getinverse <- function() m # getting inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve will take matrix in makeCacheMatrix and get it's inverse
cacheSolve <- function(x, ...) {
        m <- x$getinverse() # if inverse is set or exist- message below shows up
        if(!is.null(m)) {
                message("getting cached inverse - call cacheSolve again")
                return(m) #then returns the inverse
        }
        data <- x$get() #this is to get inverse of special matrix
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

