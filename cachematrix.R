cat("\014")

## The first function reads an input matrix, inverses it and saves matrix and inversed
## matrix into cache memory and the second function checks cache memory to see if
## the inverse of a matrix has already been calculated. If so it returns the info
## from cache memory. If not, it calculates the inverse matrix and returns it.

## Input matrix (examples)
x=matrix(c(2, 2, 4, 2, 3, 1, 5, 6, 8), nrow=3, ncol=3)
y=matrix(c(1,2,3,4),nrow=2,ncol=2)


#####################
## makeCacheMatrix ##
#####################

##   This first function makes the following actions for
##   any square invertible matrix:
##      1. Sets the matrix structure 
##      2. Gets the matrix structure 
##      3. Sets the inverse matrix
##      4. Gets the inverse matrix
##      5. Lists the four previous elements

makeCacheMatrix <- function(x=matrix()) {
        ##Save inverse Matrix into cache memory
        inverse_x <- NULL
        setinverse <- function(y) {
        ## We use here the <<- operator which can be used to 
        ## assign a value to an object in an environment that 
        ## is different from the current environment
                        x <<- y
                        inverse_x <<- NULL
        }        
        getinverse <- function() x
        setinverse_x <- function (solve) inverse_x <<- solve
        getinverse_x <- function () inverse_x 
        list (setinverse=setinverse, getinverse=getinverse, setinverse_x=setinverse_x, getinverse_x=getinverse_x)                  
}

#################
##  cacheSolve ##
#################

##  The second function either returns the cache memory inverse matrix if it has
##  been calculated before or it calculates the inverse of the original matrix
##  and returns it.

cacheSolve <- function(x,...) {
        inverse_x <- x$getinverse_x()
                if (!is.null(inverse_x)){ 
                message ("getting cached data")
                return(inverse_x)
                }       
                matrix_data <- x$getinverse()
                inverse_x<-solve(matrix_data,...)
                x$setinverse_x(inverse_x)
                inverse_x
                }