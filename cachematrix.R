## Write a short comment describing this function: 
##this function sets, gets the elements of the matrix, 
##then sets, gets the elements of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL ## initialize inverse as null
    set <- function(y) {
        x<<-y
        inv<<- NULL
    }
    
get <- function() x ##function to get matrix x
setinverse <- function(inverse) inv<<-inverse ##get function from other environment
getinverse <- function() inv ##function to get inverse of matrix
list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}


## Write a short comment describing this function:
##calculates the inverse of the matrix created in the preceding function;
##checks to see if the inverse has been calculated above; if yes then gets the inverse from cache;
##if no, performs calculation, then sets cache

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {                          ##checking whether inverse is NULL
        message("getting cached data")
        return(inv) ##returning inverse value
    
    }
    matrix_to_invert <- x$get()
    inv <- solve(matrix_to_invert, ...) ##calculates inverse value
    x$setinverse(inv)
    inv      ## Return a matrix that is the inverse of 'x'
}
