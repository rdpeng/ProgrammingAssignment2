## These are two functions that can be used to create a special object that
## stores a matrix as well as certain functions and can in unison cache its 
## inverse matrix

## The makeCacheMatrix stores stores two data objects, a matrix x and an inverse
## matrix i. It also contains four functions, set(), get(), setinv() and 
## getinv(). 

## set() assigns the value of x and i to to the parent environment (i.e., 
## assigns the input values to objects x and i in the parent environment). This 
## is done so that these object can later be called in the function cacheSolve()

## get() retrieves x from the parent environment of makeCacheMatrix as it is not
## defined within get()

## setinv() assigns the value of the inverse function to i in the parent
## environment. This occurs after cacheSolve() function runs

## getinv() retrieves i from the parent environment and is run in cacheSolve

## Finally, a list is created that contains all four functions and names them so
## that they can be called using the $ operator in the next function, 
## cacheSolve(), and returns the list to the parent environment

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(solve) i <<- solve
        getinv <- function() i
        list(set = set, get = get, getinv = getinv, setinv = setinv) 
}


## Using the functions stored in makeCacheMatrix the function cacheSolve first 
## tries to retrieve a value for i from the object passed as argument (i.e., x)
## by using the function from the macheCacheMatrix, getinv. 
## It evaluates, then, if the value of i is == to NULL. 
## If the value is not equal to NULL (i.e., there is a cached inverted matrix 
## for x) then the function returns the message "getting cached data" and 
## returns the value of i. 
## If i is equal to NULL (i.e., !isnull(i) is FALSE) the function calculates the
## inverse matrix for x, i.e., it calculates i. Then it stores the calculated 
## value of i using the setinv() function and, finally, returns the value of i.

cacheSolve <- function(x, ...) { 
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}

## Note that both functions makeCacheMatrix and cacheSolve are in a sense
## dependent on each other or rather they only make sense in conjunction.
## To function correctly, the cacheSolve function requires as input a matrix of 
## type makeCacheMatrix,i.e., a matrix that was created using the 
## makeCacheMatrix and contains the functions contained in makeCacheMatrix
