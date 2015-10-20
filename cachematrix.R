## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special matrix object stored as a listobject containing functions set(), get(), 
## setsolve() and getsolve()
makeCacheMatrix <- function(x = matrix() ) {
        ## set inverse to NULL
        s <- NULL
        
        ## overwrite object with new matrix and set cached inverse to NULL 
        ## ( y is a free variable filled by x$set(y) )
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        
        ## write current matrix to get function
        get <- function() x
        
        ## set cache s to inverse of the current matrix
        ## setsolve is called in function cacheSolve()
        setsolve <- function(solve) s <<- solve
        
        ## get cached inverse s of the matrix 
        getsolve <- function() s
        
        ## create a listobject with the functions from above which will be the new special matrix object  
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Write a short comment describing this function

## This function checks if the inverse of matrix x allready exists in cache, else it computes the inverse matrix 
## for the special matrix object created in makeCacheMatrix() and set the inverse in the cache
cacheSolve <- function(x, ...){
        ## get inverse by getsolve() of current matrix x from cache by scoping for x and 
        ## its listelement getsolve in the global enviroment 
        s <- x$getsolve()
        
        ## check if inverse matrix s exists in cache
        if(!is.null(s)) {
                # if s exist return s
                message("getting cached data")
                return(s)
        }
        
        ## else save matrix x in data by scoping for x with listelement get
        data <- x$get()
        ## compute inverse matrix of data and save in s
        s <- solve(data, ...)
        
        ## cache inverse matrix s by scoping for list x with listelement setsolve
        x$setsolve(s)
        
        ## print inverse matrix s
        s
}
