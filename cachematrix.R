## Coursera R-programming - assignment 2 
## the follwing program creates a matrix, inverts it and display the time differences between cahed and run live

## This function creates a special "matrix" object that can cache its inverse

cacheSolve <- function(x, ...) {
        ## This will return a matrix that is the inverse of 'x'
 }


makeCacheMatrix <- function(x = matrix()) 

{
        ## x: an invertible matrix
        ## result: a vector list containing functions to
        ##              1. set the matrix
        ##              2. get the matrix
        ##              3. set the inverse
        ##              4. get the inverse
        ##         this list is scoped out as the input to the 
        ##         cacheSolve() function
        
        inv = NULL
        set = function(y) 
           {
                # use `<<-` to assign a value to an object in an environment 
                # different from the current environment. 
                x <<- y
                inv <<- NULL
           }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, 
             get=get, 
             setinv=setinv, 
             getinv=getinv)
}
