## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# 
# makeCacheMatrix <- function(x = matrix()) {
# 
# }


## Write a short comment describing this function

# cacheSolve <- function(x, ...) {
#         ## Return a matrix that is the inverse of 'x'
# }
################################################################################
## 
## The name of the function is makeCacheMatrix. The input parameter for
## this function is a matrix. The output of this function consists of 
## a list with three elements. The first element is a function to cache the 
## matrix in case it is not already cached. The second element is a function that
## echoes the cache matrix, and the third element provides the function
## to get the inverse matrix either from the cache or from direct computation
## depending on whether the matrix has already been cached or not. The process
## steps are commented along the code
##
################################################################################

makeCacheMatrix <- function (x=matrix()) {
    if (!exists("p",envir=my.env,inherit=FALSE)){
        assign("p",matrix(c(1,0,0,1),nrow=2),envir=my.env)
        assign("q",matrix(c(1,0,0,1),nrow=2),envir=my.env)
    }
    ################################################################################
    ##
    ## Function to cache the matrix and its inverse in the new environment
    ##
    ################################################################################
    
    set <- function (x) {
        assign("p",x,envir=my.env) 
        assign("q",solve(x),envir=my.env)
    }
    ################################################################################
    ##
    ## Function to expose the matrix and its inverse from cache - new environment
    ##
    ################################################################################
    
    getMatrix <- function () get("p",envir=my.env)
    getInvMatrix <- function () get("q", envir=my.env)
    
    ################################################################################
    ##
    ## Here is the list to provide the entire function output (return)
    ##
    ################################################################################
    
    list(set = set, getMatrix = getMatrix,
         getInvMatrix = getInvMatrix)
}

################################################################################
##
## Function cacheSolve takes a matrix. Input is the matrix whose inverse is the
## target of output. The output of the function is the inverse of the matrix. The
## process consists of returning the inverse from the cache if that is valid.
## The function saves computational time bt storing the inverse
##
################################################################################    

cacheSolve <- function(x, ...) {
    temlist <- makeCacheMatrix(x)
    tem <- temlist$getMatrix()
    ################################################################################
    ##
    ## cacheSolve function calls makeCacheMatrix function to get the list.
    ## It examines if the matrix on hand is the same as the one in cache.
    ## The outcome of that examination leads to get the cached inverse or compute
    ## the inverse afresh.
    ##
    ################################################################################ 
    
    mateq <- is.matrix(tem) && is.matrix(x) && dim(tem) == dim(x) && all(tem == x)
    if (mateq) {
        message("getting cached data")
        return(temlist$getInvMatrix())
    }
    temlist$set(x)
    return(temlist$getInvMatrix())
}
###############################Testing Tips###################################
## Please execute the following command first: my.env <- new.env()
## x <- matrix(c(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1),nrow=4)
## will create an identity matrix of 4x4 whose inverse is itself.
## cacheSolve (x)
## run it twice - first time, it should get the computed inverse
## and the second time onwards, it should bring from cache.
## Let us create another matrix
## y <- matrix(c(11,17,19,23,29,31,37,43,53),nrow=3)
## cacheSolve(y)
## will get the inverse. First time, it should be computed. Second time
## onwards, it should be from cache.
## To test the accuracy of the inversion, set z <-cacheSolve(y)
## cacheSolve(z) Should yield y 
################################################################################ 