## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        # cached value, init NULL
        cachematrix <- NULL
        #create matrix and store in cachematrix
        set <- function(y) {
                x <<- y
                cachematrix <<- NULL
        }
        #get value of the matrix
        get <- function() x
        #inverse the matrix and store in cachematrix
        setMatrix <- function(inverse) cachematrix <<- inverse
        #get the inverted matrix
        getInverse <- function() cachematrix
        #return 
        list(set = set, get = get,
             setMatrix = setMatrix,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m   
}
