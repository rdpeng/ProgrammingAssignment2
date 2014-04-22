## ProgrammingAssignment2
## This function creates a special "matrix" object that can cache its inverse


## define the argument with default mode of "matrix"
makeCacheMatrix <- function(x = matrix()) { 
        ## initialize inv as NULL; will hold value of matrix inverse
        inv <- NULL                         
        
        ## define the set function to assign new
        set <- function(y) {                
        
        ## value of matrix in parent environment 
                x <<- y                   
        ## if there is a new matrix, reset inv to NULL 
                inv <<- NULL                
        }
        ## define the get function - returns value of the matrix argument
        get <- function() x                 
        
        ## assigns value of inv in parent environment
        setinverse <- function(inverse) inv <<- inverse  
        
        ## gets the value of inv where called
        getinverse <- function() inv                     
      
        ## you need this in order to refer 
        ## to the functions with the $ operator
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
        
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## we can use the $ operator to access the function since it was
        ## defined in the list of function pointers returned by the call
        inv <- x$getinverse()
        ## if we've already computed the mean and stored it via setinverse(),
        ## and have not invalidated the cache by calling set(), return the cached
        ## version of x
        if(!is.null(inv)) {
                message("getting cached data")
                ## we have to explicily use return here otherwise we'd keep
                ## executing the code after the if conditional ends.  Since
                ## the cached version is good, just return it and we are done.
                return(inv)
        }
        # call get() to get the underlying matrix
        data <- x$get()
        # For the underlying matrix, call setinverse function to inverse the matrix
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

##cacheSolve(makeCacheMatrix(matrix (1:4,2,2)))
##cacheSolve(makeCacheMatrix(matrix (c(-2,1.5,1,-0.5),nrow=2,ncol=2)))

