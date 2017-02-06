#Programming Assignment 2: CacheMatrix.R
# The "makeCacheMatrix" function creast the matrix and executes the following 4 steps:
makeCacheMatrix <- function(x = matrix()) {
        IM <- NULL ## Makes the variable "IM" which will eventually become the matrix inverse
        #1 Sets the value of the matrix
        set <- function(y) {
                x <<- y
                IM <<- NULL
        }
        #2 Creats "get" which gives the value of the matrix
        get <- function() x
        #3 Sets the value of the matrix inverse
        setinverse <- function(inverse) IM <<- inverse
        #4 Gets the value of the matrix inverse
        getinverse <- function() IM
        #The next line of code creates the variables for the next funtcion ("cachSolve)
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
cacheSolve <- function(x, ...) {
        IM <- x$getinverse()
        if(!is.null(IM)) {
                #In the event that the matrix inverse has been calculated, this part
                #get the result from the cache and no calculations are done. 
                message("getting cached data.")
                return(IM)
        }
        #If the matrix inverse has not been caculated, this part does the calcuations
        #to arrive the the matrix inverse. 
        data <- x$get()
        IM <- solve(data)
        x$setinverse(IM)
        # The code in the line above put the matrix inverse in the cache
        IM
}
#The following tests my functions were created correctly (formulations provided from week 3
#Discussion Forum.)
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
m1

I2 <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
I2

n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)
n1

m1 %*% n1
n1 %*% m1
solve (m1)
solve (n1)

myMatrix_object <- makeCacheMatrix(m1)

cacheSolve(myMatrix_object)

#The next code will relay the message if the functions were correclty set up
cacheSolve(myMatrix_object)

