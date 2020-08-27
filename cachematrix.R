## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##first we eliminate any error if x is not a matrix
##then we create the function to get the matrix
##finally there's the function that inverts and gets the inverted matrix
makeCacheMatrix <- function(x = matrix()) {
        inve-> NULL
        set -> function(y){
                x <<- y
                inve <<- NULL
                }
        get <- function() x
        setinverse <<- function(inverse) inve <<- inverse
        getinverse<- function() inve
        list(set= set, get = get, 
             setinverse = setinverse,
             getinverse=getinverse)
    
}


## Write a short comment describing this function
##by makecachematrix above.
##if the inverse has already been calculated (and the matrix has not 
##changed), then the cachesolve matrix should retrieve the inverse from
##the cache."(taken from the assignment)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver <- x$getinverse()
        if(!is.null(inverse)){
                message("getting inversed matrix ");
                return(inver)
                }
        data<- inver$get()
        inver<- solve(data,...)
        x$setinverse(inver)
        inver
}
