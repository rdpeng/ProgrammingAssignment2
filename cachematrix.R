## You can creates a special "matrix" object that can cache its inverse
## by using makeCacheMatrix function. And then, you can computing the
## inverse of this special "matrix".

## makeCacheMatrix returns list, that stores four functions ; 
## set, get, setInverse and getInverse.

makeCacheMatrix <- function(x = matrix()) {
    if(nrow(x)!=ncol(x) || det(x)==0){
        message("ERROR : Matrix must be a square and invertible")
        return(0)
    }
    
    I <- NULL
    set <- function(y) {
        x <<- y
        I <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) I <<- inverse
    getInverse <- function() I
    list(Set=set, get=get, setInverse=setInverse, 
         getInverse=getInverse)
    
}


## cacheSolve returns cached data if there exists stored data in x.
## Otherwise, it returns inverse of a given matrix.

cacheSolve <- function(x, ...) {
        I <- x$getInverse()
        if(!is.null(I)){
            message("getting cached data")
            return(I)
        }
        data <- x$get()
        I <- solve(data,...)
        x$setInverse(I)
        I
}
