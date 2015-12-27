## Coursera R-programming - assignment 2 
## the follwing program creates a matrix, inverts it and eventually can be used 
## to evaluate the advantages of scoping programing

## This function creates an  object (matrix()) that can cache its inverse

makeCacheMatrix <- function(x = matrix()) 

{
        ## x: a matrix THAT HIPOTHETICALY CAN BE INVERTED
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

## the following function uses the previous fucntion and caches the inverse if it has been already calculated
cacheSolve <- function(x, ...) {
        ## @x: output of makeCacheMatrix()
        ## return: inverse of the original matrix input to makeCacheMatrix()
        
        inv = x$getinv()
        
        # if the inverse has already been calculated
        if (!is.null(inv)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inv)
        }
        
        # otherwise, calculates the inverse 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(inv)
        
        return(inv)
}
##end of functions -- cooment line inseerted just to submit again
