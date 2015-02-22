## Put comments here that give an overall description of what your
## functions do

## this function make a matrix that is invertible

makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL # the inverted matrix is stored here
        # A setter function, use this to set a matrix to object created by makeCacheMatrix function
        # e.g makeCacheMatrix(testmatrix) # here we work on testmatrix
        # makeCacheMatrix$set(testmatrix1) # here we work on testmatrix1
        set <- function(y) {
                x <<- y
                inv_x <<- NULL # it also initialises xinv to null
        }
        
        get <- function() x # returning the inputed matrix
        setInv_x <- function(inv) inv_x <<- inv # set the inversed matrix
        getInv_x <- function() inv_x # return the inversed matrix
        # return a list that contains these functions, so  as to use
        # makeCacheMatrix object like this
       
        list(set = set, get = get,
             setInv_x = setInv_x,
             getInv_x = getInv_x)
}


## This function in get the inverse of matrix created by makeCacheMatrix

cacheSolve <- function(x, ...) {
        m <- x$getInv_x() # get the inversed matrix from object x 
## which will be null if it has not been done before
        if(!is.null(m)) { 
                message("getting cached data")
                return(m) # return the calculated inversion if it is not null
        }
        data <- x$get() # otherwise call x$get to get the matrix object
        m <- solve(data) # sove is called to get the inverse of the matrix
        x$setInv_x(m) # the result of the above is used by setInv_x to set the      ##object
        m # returned the soved result
}  

make_random_matrix <- function(n = 6) {
        matrix(runif(n * n), ncol=n)
}

### Code to test the functions with default of 6 for make_random_matrix
## Source this code above, then copy line 54 to the console and remove the comment sign
## this can be used to test the code with default value 

##cacheSolve(makeCacheMatrix(make_random_matrix()))



## Note: I  acknowledge the make_random_matrix
## function  from the post with this url:
## https://class.coursera.org/rprog-011/forum/thread?thread_id=429#post-2920

