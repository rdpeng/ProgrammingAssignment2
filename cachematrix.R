##Calculating the inverse of a large matrix can be very time consuming. 
##the matrix is needed again the cached value of the inverse can be retrieved, instead of re-calculating the inverse.  

##This function is a creator function. It creates a function "set" to set the value of the matrix.
##It creates a function "get" that returns the value of the matrix that is passed to the function.
##It creates a function "setinverse" that binds the value of inverse to inv in the parent environment. It creates a function "getinverse"
## that returns the value of inv. It creates a list of the 4 functions it creates.

makeCacheMatrix <- function (x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y 
                inv <<- NULL
        }
        get <- function () x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

##This funcion first determines if the inverse of a given matrix has been calculated and cached. If so, the cached value is
##returned and the message "getting cached data" is printed. If the matrix has not been previously calculated and cached, inv will
##be equal to null. The inverse of the matrix will be calculated, cached and returned.
##The argument for this function is the list created with makeCacheMatrix.  The 4 functions in makeCacheMatrix can be called 
##by indexing this list.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse() #first time cacheSolve is called with argument, inv is set to null. if called again with same argument, 
        #the value of inv in the parent environment of getinverse is returned.  This is the cached value.
        if(!is.null(inv)){
                message("getting cached data")
                return(inv) #cached inverse is returned.
        }
        data <- x$get()  #if inv = Null the inverse must be calculated. This step returns and assigns the value of the matrix to data.
        inv <- solve(data, ...) #calculates the inverse of the matrix
        x$setinverse(inv) #binds calculated inverse to inv in parent environment of setinverse-caches the value
        inv #calculated inverse is returned.
}