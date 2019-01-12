## makeCachematrix :: It makes list of different special vectors
## cachesolve :: It calculates the inverse of matrix after checking condition.
## Set :: It set the vector value
## Get :: It get the vector value
## Setinverse :: It set the inverse
## getinverse :: It get the inverse

## makeCacheMatrix function makes a list of function such as, set, get, 
## setinverse, and getinverse which take the value of vector 
## this function make the cache matrix which stores already calculated inverse..

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) {
                i <<- inverse
        }
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)


}


## cacheSolve function checks the condition whether the matrix is inverted or not. 
## If the matrix is inverted already, then it display the result from cached storage.
## If matrix is not inverted then it calulates the inverse of matrix and display it. 
## It also set calculated inverse to setinverse function.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        else {
                i <- solve(x$get(), ...)
                x$setinverse(i)
                return(i)
        }
        ## Return a matrix that is the inverse of 'x'
}
