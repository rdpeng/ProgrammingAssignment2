#### There are two functions 
#### Together, they will be able to calculate the inverse of a invertiable matrix
#### Function makeCacheMatrix will store the inverse of the matrix
#### Function cacheSolve will first check if the inverse of the matrix has been calculated
#### or not. If the inverse already exists, then it returns the stored value of the inverse
#### Otherwise, it will calcute the inverse and store the value for future.
#### I am not sure whether I am required to the matrix is different from previously or not.
#### The cacheSolve function does not do that
#### However, I have some code at the end which will do the job. These codes are commented out.

rm(list=ls())
#### This is used to store the inverse value 
makeCacheMatrix <- function(x=matrix()) {           
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }  
        # When the set function is called,
        # it will assign the value of the arugment as the matrix in the environment of the makeCacheMatrix function
        # It will also assign NULL as the value to m in the environment of the makeCacheMatrix function 
        # The set function returns nothing.
        get <- function() x # When this is called, it returns x.
        setinverse <- function(inverse) m <<- inverse 
        # When setinverse function is called, assign the value of inverse to m in the environment of the makeCacheMatrix function
        getinverse <- function() m # When called, return the value of m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) # makeCacheMatrix function returns a list of these four functions
}

#### This is used to check if the inverse has been calculated.
#### If yes, returns the existing value.
#### If not, calculate the inverse of the matrix and cache it.
#### The code does not check if the given matrix is the same as before or not (if the inverse exists).
cacheSolve <- function(x) {
       
        m <- x$getinverse() # This calls the makeCacheMatrix and the getinverse functions in it.
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
                # if the inverse exists (m is not NULL), then shwos the message and return m as the inverse
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}

### Illustrate how to call the cacheSolve function.
input = matrix(c(1,0,0,1),2,2) # the matrix to calculate inverse for
x = makeCacheMatrix(input) 
cacheSolve(x) # The first time when you call this, it will show the inverse for the input.
cacheSolve(x) # The second time, it will show the message "getting cached data", which means the inverse is not calculated but got from the cache.


###### This is just for fun. Comment out Lines 53/54 before running the code below
# equal <- function(x, y) is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y) # Function to check if two matrices are identical
# inverse = cacheSolve(x)
# 
# newinput = matrix(c(1,2,1,1),2,2)
# ##@ If the new input matrix is the same as before, then skip the calculation and return the cached inverse
#         if (equal(input,newinput)==TRUE){
#                 message("getting cached data")
#                 inverse = inverse
#         }else{
# ### otherwise, recalcute the inverse and update the input in the meantime
#                 message("calculating")
#                 x = makeCacheMatrix(newinput) 
#                 inverse = cacheSolve(x)
#                 input = newinput
#         }