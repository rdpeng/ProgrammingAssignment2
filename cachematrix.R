######  ASSIGNMENT-2- Caching the Inverse of a Matrix #########################
# This leverages the thought process demonstrated in " Caching the Mean       
# of a Vector" example shown on the assignment page. 
# ASSUMPTION: Input matrix is a square invertible matrix.
#
#                      Thanks for your evaluation and feedback !!! 
###############################################################################

# The first function "makeCacheMatrix", is a function that creates 4 functions
# and stores them in a list of functions.To use functions created within the 
# makeCacheMatrix function, we need to call-out these functions in a special 
# way- makeCacheMatrix + $ + 2nd function + argument. More will be shown below. 

####################### Explanation of "makeCacheMatrix" Function#############

# Function "makeCacheMatrix" is the main function with 4 functions in it
# variable "inver" captures the inverse of matrix and intiallized to NULL here
# Function1- "Set" function changes the matrix stored in the main function. 
# Superassignment operator <<- replaces the value of x (matrix) in the main 
# function leveraging R's Lexical Scoping. It also resets "inver" variable
# to NULL if there is a change in the matrix as the old inverse in no longer
# valid and needs to be computed again in function cachesolve.
# Function2- "get" <- function () x, simply returns the matrix x stored in the 
# main function.
# Function 3- "setinverse" assigns the value of "inverse" to the "inver" in the
# main function due to superassignment (Lexical Scoping). it does not compute
# anything. computation happens in the "cachesolve" function. 
# Function 4- "getinverse" simply returns the value of inverse matrix- "inver".
# The last step of "makeCacheMatrix" is to make a list of 4 funtions via the 
# list commpand function. 

##################### Explanation of "cachesolve" Function ###################
# "cachesolve" is the function where the inverse is actually computed, after
# confirming that the inverse is not existing in the cache. The input to 
# "cachesolve" is the object where "makeCacheMatrix" function is stored. As
# a first step in this function, "inver" is retrieved from the "makeCacheMatrix"
# function list by "x$getinverse()" function.Next a check is performed in the 
# "if loop" to check if the inverse is not NULL. If inverse is not NULL, message
# "getting cached data" is displayed and "inver" value stored in the 
# cache is returned. If however, "inver" comes out to be NULL, that means it
# has not been computed for the given input matrix yet, the matrix data is 
# retrived in the "cachesolve" function using the "x$get()" function. After
# the data is retrived, "inver" is computed using the "solve" function to this 
# data. In the next step, inverse value is set (cached) to "inver" in the 
# main function "makeCacheMatrix" by using the "x$setinverse(inver)" and "inver"
# value is returned as the inverse of the matrix at the end of processing. 

################## Testing of this Code #######################################
# After running the entire code here, i.e. Ctrl+A and then "Run"
# 1. Please create a matrix by using x <- rbind (c(1,2), c(3,4)) or any 
# other square invertible matrix. 
# 2. Assign "makeCacheMatrix" to a variable (such as 'm') and pass 'x' has input
# m <- makeCacheMatrix(x)
# 3. Check the matrix 'x' is assigned withing the main function by using
# m$get(), you should see the following- 
#       [,1] [,2]
# [1,]    1    2
# [2,]    3    4
# 4. Now compute inverse of this matrix by using cachesolve(m). Print it.
# You should see below. Remember since, the inverse is being computed the first
# time, we don't see any messages that the cached data is being retrieved
#       [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5
# 6. Run "cachesolve(m) again and you see will the cache data is being pulled
# now. The inverse doesn't change. 
#cachesolve(m)
#getting cached data
#       [,1] [,2]
#  [1,] -2.0  1.0
#  [2,]  1.5 -0.5
# 7.Now let's try to assign a new matrix 'y' y <- rbind( c(6,7), c(8,9))
# by using  m$set(y)
# 8. Check that matrix is updated in the main function by m$get()
# 9. Now again compute Inverse by cachesolve(m). you will see that the new 
# inverse has been computed. 
# 10. Finally, type and enter "cachesolve(m)" one more time to see that cached
# data is being retrieved now again. 

### A quick test to verify that the inverse matrix is correct is to do a TRUE
### matrix multiplication using '%*%' operator. Multiplication of a matrix and
### it's inverse should be an Identity matrix, i.e. maindiagnoal will have '1s'
### and rest of the values will be '0s'. Try " y %*% cachesolve(m)" to see.
####################### Thanks for evaluating my work #######################

makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL  
        set <- function(y) {
                x <<- y
                inver <<- NULL
        } 
        get <- function() x
        setinverse <- function(inverse) inver <<- inverse
        getinverse <- function() inver
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

cachesolve <- function(x, ...) {
        inver <- x$getinverse()
        if(!is.null(inver)) {
                message("getting cached data")
                return(inver)
        }
        data <- x$get()
        inver <- solve(data, ...)
        x$setinverse(inver)
        inver
}

####################### Thanks for evaluating my work #######################
