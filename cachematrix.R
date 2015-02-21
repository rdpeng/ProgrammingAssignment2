## These functions try to optimize matrix inversion by caching


## This function creates a matrix and stores it's inverse

makeCacheMatrix <- function(x = matrix()) 
{
    inv <- matrix()
    set <- function(y) {
        x   <<- y
        inv <<- matrix()
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## The below function takes the input data from user and checks if the inverse
## is cached. If not it calculates the inverse and calls the above function
## to stores it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinv()
    
    if(is.numeric(inv)) 
    {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    square_data=data %*% data
    inv <- solve(square_data, ...)
    x$setinv(inv)
    inv
    
}
