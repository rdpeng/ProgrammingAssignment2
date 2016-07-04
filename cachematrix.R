# The following are two functions that involve matrices. 
# The first function stores the matrix in a cache allowing it to be retrieved 
# at a later point in another function. 
# The second function uses the matrix stored in the first function to find it's 
# inverse. 

# In order to use these functions, you first need to create a square matrix.
# Here are two examples of square matrices that I have created: "A" & "B"

A <- matrix(1:4, 2, 2) 
# "A" is a 2x2 matrix containing the numbers 1:4
B <- matrix(runif(16,0,100),4,4) 
# B is a 4x4 matrix containing random numbers between 0 and 100

# In order to use cacheSolve, the input into makeCacheMatrix has to be a square
# matrix. 

makeCacheMatrix <- function(x = matrix()) {
        matr <- NULL
        set <- function(y) {
                x <<- y
                matr <<- NULL
        }
        get <- function() x
        setsolver <- function(solve) matr <<- solve
        getsolver <- function() matr
        list(set = set, get = get,
             setsolver = setsolver,
             getsolver = getsolver)
}


# This function will find the inverse of the matrix stored in makeCacheMatrix 
# function. 
# The input for cacheSolve has to be makeCacheMatrix or a value stored of 
# makeCacheMatrix.

cacheSolve <- function(x, ...) {
        matr <- x$getsolver()
        if(!is.null(matr)) {
                message("obtaining cached matrix inverse")
                return(matr)
        }
        data <- x$get()
        matr <- solve(data, ...)
        x$setsolver(matr)
        matr
}








