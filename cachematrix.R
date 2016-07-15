## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL  ## sets an empty variable which will be populated later
        set <- function(y) {
                x <<- y  ## if x exists in the parent env, assign the value of y to x
                m <<- NULL ## if m exists in the parent env, clear its class and values
        }
        get <- function() x ## assigns an expression of x to get
        setCacheMatrix <- function(solve) m <<- solve  ## create an object called setCacheMatrix
                                                        ## that will run solve against
                                                        ## whatever is within the 'm'
                                                        ## matrix
        getCacheMatrix <- function() m  ## create a function called getCacheMatrix that will
                                        ## display the matrix m
        list(set = set, get = get,  ## assigns list names and values
             setCacheMatrix = setCacheMatrix, 
             getCacheMatrix = getCacheMatrix)
}

## Dave's help
fn <- function() 1
## SAME AS fn <- function() { 1 }
## <<- operator will walk through each parent environment 
## to look for the variable (in the example above x)

## In the get function, all we are doing is saying that get will get 
## the x variable from the parent environment where it is located
## set and get are "accessors" aka "getters and setters"...these are 
## often used to get variables and/or set variables within a function

## Write a short comment describing this function

cacheSolve <- function(x, ...) {  ## Return a matrix that is the inverse of 'x'
        
        m <- x$getCacheMatrix()  ## assign the 
        if(!is.null()) {  ## if m is not null, print a message
                message("getting cached data")
                return(m)  ## return m to the cacheSolve function
        }
        matrix <- x$get()  ## retrieve the function that is within the list in the get() object
        m <- solve(matrix, ...)  ## do matrix multiplication and assign the result to m
        x$setCacheMatrix(m)  ## write m to the list at setCacheMatrix
        m
}
