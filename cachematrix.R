##Caching the inverse of a matrix
##The below functions create a special matrix and invert the special matrix

## This function creates a special matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)

}




## This function inverts the special matrix, but it checks to see if the inverse is already calculated 

cacheSolve <- function(x, ...) {
   	 inv <- x$getinverse()
    	 if(!is.null(inv)) {
       message("getting cached data.")
       return(inv)
    	 }
    	 data <- x$get()
    	 inv <- solve(data)
    	 x$setinverse(inv)
    	 inv
}

