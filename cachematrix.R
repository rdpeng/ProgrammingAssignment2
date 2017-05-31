# R programming : WEEK 3 
# Caching the inverse of a matrix 



## The following function creates a special "matrix" object that can cache its inverse.
# It takes as argument a matrix and returns an object that contains a list of functions
# to set the value of the matrix, get the value of the matrix, set the inverse matrix and 
# get the inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL 
        set <- function(y){  # here it is in case the value changes, we put the inv back to NULL
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse 
        getinv <- function() inv
        list(set=set,get=get,setinv=setinv,getinv=getinv)
        
}


## In the following function, we first check if the inverse matrix has already been calculated. 
# If yes, it takes the value from the getinv function and returns it
# If not, it calculates the inverse matrix and set it in cache 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data = x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}

# here are a few lines to test the code (it seems to work pretty well)

m = matrix(rnorm(9),3,3)
n = makeCacheMatrix(m)
cacheSolve(n) # here we see that the first time we run this command line, it calcualtes the inv
# if we run it a second time it writes "getting cached data"





