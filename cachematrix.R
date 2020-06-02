# The comments are written beside my code as much as I can 

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL                 # Assumes the input matrix is invertible
        assign <- function(mat){        # Assigning value of matrix
                matrix <<- mat          # Double arrow assignment
                inverse <<- NULL        # Operator which can be used to assign a value to an object in an environment that is different from the current environment.
        }
        get <- function() {x}           # Getting the value of the matrix
        assigninverse <- function(inv) {inverse <<- inv}         # Assigning the value of inverse matrix
        getinverse <- function() {inverse}                      # Getting the value Of inverse matrix
        list(get = get, assign = assign, assigninverse = assigninverse, getinverse = getinverse) #creating the list 
}


## Write a short comment describing this function
## Returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        invmatrix <- x$getinverse()        # Returns a matrix that is inverse of x
        
        if( !is.null(matrix)){             # Checking is inverse has already been calculated, if yes, could be retrieved from cache
                message("getting cached data") 
                return(invmatrix)
        }
        data <- x$get()                    # Getting matrix from our object
        invmatrix <- solve(data) %% data() # Calculating the inverse
        x$assigninverse(invmatrix)         # Assigning the inverse matrix
        return(invmatrix)
}
