## Put comments here that give an overall description of what your
## functions do

## This first function take a matrix and apply de solve function for make an inverse of it

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        #
        get <- function() x
        #
        setinverse <- function(solve) i <<- solve
        #
        getinverse <- function() i
        #
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse )

}


## This function cache the inverse of the matrix of the above function and show the result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        #
        if(!is.null(i)){
                message("Getting Cahe Data")
                return(i)
        }
        #
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
