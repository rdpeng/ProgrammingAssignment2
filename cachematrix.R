## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

	invert <- NULL
    	set <- function(y) {
        x <<- y
        invert <<- NULL
    	}
    get <- function() x
    setInverse <- function(inverse) invert <<- inverse
    getInverse <- function() invert
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  }



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
m<-x$getinverse()
        if(!is.null(m)){
                message("getting cached matrix inverse")
                return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setinverse(m)
        m
}
