## The program creates two functions to allow us to calculate and set 
## the value of the inverse of a matrix in the cache and get it from there 
## if the inverse has already been calculated (and the matrix has not changed)
## 
## How we can work as caching values?
## Having variables at two levels allows us to maintain state across function invocations. 
## This is possible because while the function environment is refreshed every time,
## its parent environment stays constant. 
## The key to managing variables at different levels is the double arrow assignment operator (<<-). 
## Unlike the usual single arrow assignment (<-) that always assigns in the current environment, 
## the double arrow operator will keep looking up the chain of parent environments until it finds a matching name
##
## In our code the functions developed are:
##      makeCacheMatrix -- a function of 'x', where 'x' is a matrix.
##      The function creates a new object, a list of 4 enclousure functions, 
##      to store in the cache the value of x and its inverse
##      and get the value of the 'x' and its inverse from the cache.
##      Note that their enclosing environment is the environment created when makeCacheMatrix is run
##
##      cacheSolve -- a function of 'x' where 'x' is an object returned 
#       by makeCacheMatrix function
##      The function calculates the inverse of the 'x' and store it in the cache
##      if it is the first time, in other case it gets the inverse from the cache 
##
## Example of usage:
## create a new object of type "makeCaheMatrix", in the end a list of 4 elements
##      m <- makeCacheMatrix()
## assign the value of the matrix that we want to cache its inverse
##      m$set(matrix(1:4,2,2))
## call the function cacheSolve to calculate  
## the inverse and send it to the cache if it is the first time or
## get from the cache if it is calculated before
##      cacheSolve(m)




## 'makeCacheMatrix' function as a function of 'x', where 'x' is a matrix
## makeCacheMatrix returns a list of 4 functions 
## This 4 functions are: 
##      set -- to assign the value of the matrix to an object in an environment 
##      that is different from the current environment (to cache it)
##      get -- to get the value of the matrix from the cache
##      setinv -- to assign the value of the matrix's inverse to an object in 
##      an environment that is different from the current environment (to cache it)
##      getinv -- to catch the value of the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setinv <- function(inv) s <<- inv
    getinv <- function() s
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}



## cacheSolve -- computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache
## Note: 
##		If 'x' is not a invertible matrix, R will return this error: 
##      "Error in solve.default(x) : Lapack routine dgesv: system is exactly singular: U[,] = 0".
##		If 'x' is not a square matrix, R will return thir error: 
##      "Error in solve.default(x) : 'a' (r x c) must be square".

cacheSolve <- function(x, ...) {
    s <- x$getinv()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setinv(s)
    ## Return a matrix that is the inverse of 'x'
    s
}
