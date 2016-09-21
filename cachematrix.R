## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL #initialise inverse variable and assign a NULL to it
        
        # x comes from the parent environment. Here we assign it a value
        set <- function(y = matrix()) {
                x <<- y 
                inver <<- NULL #In case inver has already been set, it will be Null-ed here
        }
        
        
        get <- function() x
        
        #Here I assign a value to the inver variable in the parent environment
        setInver <- function(invers){
                inver <<- invers
                return(inver) #check what the inver value from the parent env is
        } 
        
        #
        getInver <- function() inver
        
        list(set = set, get = get, setInver = setInver, getInver = getInver)
}


## Write a short comment describing this function

## Return a matrix that is the inverse of the special matrix returned by makeCacheMatrix above. check whether inversion has been done is included as well
cacheSolve <- function(x = makeCacheMatrix, ...) {
        invrs <- x$getInver()
        
        #In case a cache matrix is availale the code within the if is executed
        if(!is.null(invrs)) {
                message("Getting cached inversed matrix")
                return(invrs)
        }
        
        #In case there is no cached matrix, it is going to be inverted
        matrixSolveFunct <- x$get()
        invrs <- solve(matrixSolveFunct, ...)
        x$setinverse(invrs)
        invrs #returns the inversed matrix
}
