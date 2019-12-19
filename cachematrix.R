## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix creates a special "vector", which is really a list containing a function to
# 
# set the value of the vector
# get the value of the vector
# set the value of the inverse
# get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL ## Initialize inverse
                set <- function(y) {   #set matrix
                        x <<- y
                        inv <<- NULL
                }
                get <- function()x  #get matrix
                setinv <- function(inverse) inv <<- inverse  #set the inverse of matrix
                getinv <- function() inv    #get inverse of the matrix 
                list(set = set, get = get,   #return a list of vectors
                     setinv = setinv, 
                     getinv = getinv)

}


## Write a short comment describing this function
# The following function calculates the inverse of the matrix created with the above function. 
# However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the matrix and sets the inverse in the cache via the setinverse function.


cacheSolve <- function(x, ...) {
                inv <- x$getinv()        #return a inverse matrix of x
                if(!is.null(inv)) {
                        message("getting cached data")   #return cached data if inverse is calculated 
                        return(inv)
                }
                data <- x$get()       #get matrix
                inv <- solve(data)    #calculate inverse 
                x$setinv(inv)         #set inverse to object
                inv
                ## Return a matrix that is the inverse of 'x'
}
                
              
x <- matrix(runif(50*50), nrow = 50, ncol = 50) #set a 50*50 matrix
x_obj <- makeCacheMatrix(x)
x_inv <- cacheSolve(x_obj)  # get inverse first time 
head(x_inv)
x_inv <- cacheSolve(x_obj) # get inverse from cache 
head(x_inv)


        
