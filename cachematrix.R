## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
#The <<- operator which can be used to assign a value to an object in an environment that is different 
#from the current environment. Below are two functions that are used to create a special object that 
#stores a numeric matrix and cache's its inverse.
#The first function, makeCacheMatrix creates a special "Matrix", which is really a list containing a function to
#1.     set the value of the Matrix
#2.     get the value of the Matrix
#3.     set the value of the inverse
#4.     get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    InvM <- NULL
    set <-function(y){
        x <<- y
        InvM <<- NULL   
    }
    get <- function(inverse) x
    setInvM <- function(inverse) InvM <<- inverse
    getInvM <- function() InvM
    list(set=set, get=get,
         setInvM=setInvM,
         getInvM=getInvM)
}
## Write a short comment describing this function
# 
#The following cacheSolve function calculates the inverse of the special "Matrix" created 
#with the above function. However, it first checks to see if the inverse has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the data and sets the value of 
#the inverse in the cache via the setInvM function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    InvM <- x$getInvM()
    if (!is.null(InvM)){
        message("getting cache data")
        return(InvM)
    }
    data <- x$get()
    InvM <- solve(data, ...)
    x$setInvM(InvM)
    InvM
    
}

 ## Test run:
# x <- matrix(rnorm(9), nrow = 3, ncol = 3)
# ## x <- matrix(rnorm(4), nrow = 2, ncol = 2)
#  m = makeCacheMatrix(x)
#  m$get()
#           [,1]        [,2]      [,3]
#[1,] -0.8721588 -1.85374045  0.184926
#[2,] -0.1017610 -0.07794607 -1.379944
#[3,] -0.2537805  0.96856634 -1.435514
# cacheSolve(m)
#            [,1]       [,2]        [,3]
#[1,] -0.87069693  1.4919527 -1.54636210
#[2,] -0.12270252 -0.7808101  0.73477703
#[3,]  0.07113854 -0.7905842  0.07252933
#cacheSolve(m)
#getting cache data
#            [,1]       [,2]        [,3]
#[1,] -0.87069693  1.4919527 -1.54636210
#[2,] -0.12270252 -0.7808101  0.73477703
#[3,]  0.07113854 -0.7905842  0.07252933
