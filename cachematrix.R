## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix gives an interface to operate the functions on any data.

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL;                            # initialize variable "inv" to NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x                     # used to get the value of variable "x"
        setinv <- function(solve) inv <<- solve # used to set the value of variable "inv" after doing inverse
        getinv <- function() inv                # used to get the value of variable "inv"
        list(set = set, get = get, setinv = setinv, getinv = getinv)    # used to define the list of functions so that the functions (like "get", "set", "getinv" etc) within main function "makeCacheMatrix" can be used effectively. e.g. step1: save main function "makeCacheMatrix" into a variable say "var": var <- makecacheMatrix(); step2: now we can use subfunctions of main function like var$get()
}


## Write a short comment describing this function
# This function takes the function called makeCacheMatrix as an input. e.g. cacheSolve(makeCacheMatrix(inputmatrix));
# variable "inputmatrix" contains the matrix whose inverse needs to be calculated. "inputmatrix" is passed to makeCacheMatrix function which is then executed.
# functions within "makeCacheMatrix" like get, set, getinv, setinv, can be accessed like inputmatrix$get().
# First this function gets the inverse of matrix using getinv(). if inverse matrix variable "inv" is cached in R then return it else calculate and set it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()                       # get the value of variable "inv".
        if(!is.null(inv)) {                             #
                message("getting cached data")          # IF LOOP: if "inv" is NULL, then don't run this loop and calculate
                return(inv)                             #          the inverse of the matrix in the next step. If "inv" is
        }                                               #          cached in the memory of R then return the value of "inv"
        data <- x$get()                         # get the input matrix whose inverse is to be calculated
        inv <- solve(data, ...)                 # calculate the inverse of the input matrix and save it in "inv" variable
        x$setinv(inv)                           # after calculating inverse of input matrix in "inv" variable, cache it in R by using setinv function
        inv                                     # print inv
}
