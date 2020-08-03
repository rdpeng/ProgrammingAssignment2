## Put comments here that give an overall description of 
## what your functions do

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## The function is made up of four more functions, the
## first function "set" receives as an argument the matrix
## that we want to calculate the inverse of, then proceeds 
## to assign the matrix to the variable x that was originally
## created as 'x = matrix ()', as I take a value within the
## function, if we apply the concept of lexical scope, the 
## function get () that shows the variable x, will always 
## look for this within the environment where the function is
## defined, otherwise if it had not been this way we could Call
## the get () function and it would have searched our global 
## environment. Then it has two more functions, setinv () saves 
## the inverse supplied by the user, and finally getinv () that 
# shows the inverse previously entered.

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setinv(inv)
        inv
}
## This function is related to makeCacheMatrix, because it calls its 
## functions within it, with the aim of verifying if the inverse of 
## the cache we want to calculate is found in our cache, if it 
## verifies that if it exists it sends the user to review the cache
## and print that value in reverse; on the other hand, if there is 
## no such value, it would be in charge of calculating the inverse of
## the matrix previously supplied and print said calculation.