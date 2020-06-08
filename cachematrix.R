## We create a matrix. Check if it has an inverted matrix. If not we set it.
##Now that we have the inverse created, we dont need to create it again for
##this matrix

## 
## Used for assigning the matrix. Suppose we set a 2*2 matrix.Then we can get
##this matrix. And then check if its inverse has been created if not we call
#the cache solve function to create the inverse.


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y            ##sets the matrix
        inv <<- NULL       ##assigns its  inverse as null as it has not been calculated yet
    }
    
    get <- function() {
        x   ##gets the matrix
    }
    
    setInverse <- function(inverse) {
        inv <<- inverse  ##sets the inverse
    }
    
    getInverse <- function() {
        inv ## gets the inverse
    }
    
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## we calculate the inverse for matrix here which already doent have an inverse
## if it does have an inverse we simply display it


cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    
    if(!is.null(inv)){
        message("getting the cached data for this")
        return(inv)
        }
       
    dummy <- x$get()
    inv <- solve(dummy, ...)
    x$setInverse(inv)
    inv
}
