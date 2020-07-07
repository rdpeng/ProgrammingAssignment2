## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    #set the matrix function 
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    #get the matrix
    get <- function() {x}
    #set the inverse matrix
    setinverse <- function(solve) {m <<- solve}
    #get the inverse matrix
    getinverse <- function() {m}
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    #see if the inverse is on cache
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    #calculate the inverse
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
