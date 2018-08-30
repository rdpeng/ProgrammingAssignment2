## A pair of functons that will do the inverse of a matrix

## MakeCacheMatrix is a function that stores the matrix inverse

makeCacheMatrix <- function(x = matrix()) {

        inve <- NULL
        set <- function(y){
                x<<- y
                inve <<- NULL
        }
        get <- function() x
        setInverse <- function(inverseMatrix) inve <<- inverseMatrix
        getInverse <- function() inve
        list( set = set,get = get,setinverse = setinverse,getinverse=getinverse)
}


## cacheSolve function inverse the matrix if it is not already stored in cache else provide the inverse value stored in cache

cacheSolve <- function(x, ...) {
        inve <- x$getInverse()
        if(!is.null(inve)){
        message("getting cached data")
                return(inve)
        }
        data <- x$get()
        inve <- solve(data)
        x$setInverse(inve)
        inve
}
