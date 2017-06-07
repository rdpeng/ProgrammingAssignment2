## Put comments here that give an overall description of what your
## functions do :   "Write a pair of functions that cache the inverse of a matrix."



## Write a short comment describing this function:

## "This function creates a special "matrix" object that can cache its inverse."

makeCacheMatrix <- function(x = matrix()) {
    m <- matrix()
    set <- function(y) {
        x <<- y
        m <<- matrix()
    }
    get <- function() x
    setinverse <- function(theinverse) m <<- theinverse #Seems that 'inverse' is a reserved word...
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## Write a short comment describing this function:

## "This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache."

ca <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    #print(m)
    if(!is.na(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    #print(data)
    if (det(data)){ #i.e. if det(data) != 0
        m <- solve(data)
        x$setinverse(m)
        m
    }
    else{
        message("the determinant is zero")
        return (m)
    }
        
}
